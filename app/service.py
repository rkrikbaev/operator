import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time, os
import requests, json

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, point, port)->dict:

        # time delay to deploy application in the container
        time.sleep(2)

        url = f'http://{point}:{port}/action'
        logger.debug(f'query url: {url}')
        logger.debug(f'query payload: {payload}')
        response = {
                    "state": 'executed',
                    "point": point,
                    "start_time": str(datetime.datetime.now())
                    }
        try:
            # os.system("""'curl --location --request POST 'http://almaty2:8005/action' --header 'Content-Type: application/json' --data-raw '{"data": [[1626321114000],[1626321115000],[1626321116000]]}'""")
            result = requests.post(url, data=json.dumps({'data':payload}))
            # result = requests.request('POST', url=url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}))
            # result = mureq.post('http://almaty2:8005/action', body=b'{"data":payload}')

            response["finish_time"] = str(datetime.datetime.now())
            response['response'] = result.json()
            logger.debug(response)
        
        except Exception as exp:
            response['state'] = 'error'
            logger.debug(exp)

        return response 

class DockerOperator():
    """
    Class to work with docker objects
    """
    def __init__(self,):

        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        except DockerException as exc:
            logger.error(f'Connection with docker.socket aborted {exc}')
            raise exc

    def deploy_container(self, point, service_config, model_features, tracking_server, model_id, regressor_names):

        image = service_config.get('image')
        cpuset_cpus = service_config['limits'].get('cpuset_cpus')
        con_mem_limit = service_config['limits'].get('con_mem_limit')
        network = service_config.get('network')
        startup = service_config.get('startup')

        if model_features is None:
            model_features = service_config.get('features')
        
        try:
            container = self.client.containers.get(point)
            container.remove(force=True)
            logger.debug(f'Delete exist container with same point name: {point}')
            time.sleep(2)
        except NotFound:
            pass

        logger.debug('Try to create container')
        logger.debug(f'{point},{service_config},{model_features},{tracking_server},{model_id},{regressor_names}')            
        
        try:
            container = self.client.containers.run(
                image,
                name=point,
                volumes=['/usr/local/etc/mlruns:/application/mlruns'], 
                detach=True, 
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network=network,
                environment=[
                    f'FEATURES={model_features}', 
                    f'TRACKING_SERVER={tracking_server}', 
                    f'MODEL_URI={model_id}',
                    f'REGRESSORS={regressor_names}',
                    f'PATH_TO_MLRUNS=/application/mlruns'
                    ],
                command='gunicorn -b 0.0.0.0:8005 app:api --timeout 600'
                )
            container_id = container.short_id
            
            wait_counter = 0
            while wait_counter < startup:
                container = self.client.containers.get(container_id)
                container_state = container.status.lower()
                # container_state = container.attrs['State'].get('Status').lower()           
                if container_state == ['created']:
                    time.sleep(1)
                    wait_counter += 1
                elif container_state in ['running']:
                    logger.debug(f'container #{container_id}, started')
                    break                
            else:
                raise RuntimeError(f'Fail to deploy container for point {point}')

        except Exception as exc:
            logger.error(str(exc))
        
        return {'id':container_id, 'service_ip':point, 'state': container_state}

    def remove_container(self, container_id):
            
        '''
        Remove a docker container using a given id; passing keyword arguments
        documented to be accepted by docker's client.containers.remove function
        No extra side effects. Handles and reraises APIError exceptions.
        '''
            
        try:               
            container = self.client.containers.get(container_id)
            container.remove(force=True)
            logger.debug(f'container {container.short_id} removed')
        except APIError as exc:
            logger.error(f'unable to remove container by APIError error: {exc}')