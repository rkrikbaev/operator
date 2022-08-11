import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time, os
import requests, json
from requests import ConnectionError


from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, container_id, port, point, ip_address)->dict:

        tries = 0
        logger.debug('Try to call the model first time')

        while tries < 5:
            url = f'http://{ip_address}:{port}/health'
            logger.debug(f'query url: {url}')
            
            try:
                health = requests.get(url, timeout=2)
            except Exception as exc:
                logger.debug(f'query /health fail by: {exc}')
            
            if health.ok:
                
                url = f'http://{ip_address}:{port}/action'

                logger.debug(f'query url: {url}')
                logger.debug(f'query payload: {payload}')

                start_time = str(datetime.datetime.now())
                
                try:
                    # os.system("""'curl --location --request POST 'http://almaty2:8005/action' --header 'Content-Type: application/json' --data-raw '{"data": [[1626321114000],[1626321115000],[1626321116000]]}'""")
                    result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}))
                    # result = requests.request('POST', url=url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}))
                    # result = mureq.post('http://almaty2:8005/action', body=b'{"data":payload}')

                    logger.debug("response executed")

                    return {
                            "state": 'predict executed',
                            "point": point,
                            "start_time": start_time,
                            "finish_time":  str(datetime.datetime.now()),
                            "predict": result.json()
                            }
                
                except ConnectionError as exp:
                    logger.debug(str(exp))
                    return {
                            "state": 'predict caused error',
                            "point": point,
                            "start_time": start_time,
                            "text": str(exp)
                            }
            else:
                logger.debug(f'query /health success: {health.ok}')
                logger.debug('trying request model again')
                tries += 1
                time.sleep(tries)
        else:
            logger.debug(f'fail to call the model for point {point}')
            raise RuntimeError(f'fail to call the model for point {point}')


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

    def deploy_container(self, point, service_config, model_features, model_id, regressor_names):

        image = service_config.get('image')
        cpuset_cpus = service_config['limits'].get('cpuset_cpus')
        con_mem_limit = service_config['limits'].get('con_mem_limit')
        network = service_config.get('network')
        startup = service_config.get('startup')
        ip_address = None

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
        logger.debug(f'{point},{service_config},{model_features},{model_id},{regressor_names}')            
        
        try:
            network='models'

            container = self.client.containers.run(
                image,
                name=point,
                volumes=['/usr/local/etc/mlruns:/application/mlruns'], 
                detach=True,
                ports={8005:8005},
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network=network,
                environment=[
                    f'FEATURES={model_features}', 
                    f'TRACKING_SERVER=http://mlflow:5000', 
                    f'MODEL_URI={model_id}',
                    f'REGRESSORS={regressor_names}',
                    f'PATH_TO_MLRUNS=/application'
                    ],
                command='gunicorn -b 0.0.0.0:8005 app:api --timeout 600'
                )
            container_id = container.short_id
            
            wait_counter = 0
            while wait_counter < startup:
                container = self.client.containers.get(container_id)
                container_state = container.status.lower()
                           
                if container_state == ['created']:
                    time.sleep(1)
                    wait_counter += 1

                if container_state in ['running']:
                    ip_address = container.attrs['NetworkSettings']['Networks'][network]['IPAddress']
                    logger.debug(f'container #{container_id}, started with IP: {ip_address}')
                    break                
            else:
                raise RuntimeError(f'Fail to deploy container for point {point}')

        except Exception as exc:
            logger.error(str(exc))
        
        return ip_address, container_id, container_state

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