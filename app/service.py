from dotenv import load_dotenv
from pathlib import Path

import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time, os
import requests, json
from requests import ConnectionError

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

dotenv_path = Path('.env')
load_dotenv(dotenv_path=dotenv_path)

PATH_TO_MLRUNS = os.environ.get('PATH_TO_MLRUNS')
print()

class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, container_id, port, point, ip_address)->dict:

        tries = 0
        health_ok = False
        logger.debug('Try to call the model first time')

        session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=100,
            pool_maxsize=100)

        session.mount('http://', adapter)
        url = f'http://{ip_address}:{port}/health'

        while tries < 15:
          
            try:
                health = session.get(url, timeout=600)
                health_ok = health.ok
                logger.debug(f'query url: {url} status: {health_ok}')

                return 

            except ConnectionError as exc:
                logger.debug(f'query /health fail by: {exc}')
                tries += 1
                logger.debug(f'query /health success: {health_ok}: tries: {tries}: sleep: {tries*tries} sec')
                time.sleep(tries)
                
            url = f'http://{ip_address}:{port}/action'

            logger.debug(f'query url: {url}')
            logger.debug(f'query payload: {payload}')

            start_time = str(datetime.datetime.now())
            
            try:
                # os.system("""'curl --location --request POST 'http://almaty2:8005/action' --header 'Content-Type: application/json' --data-raw '{"data": [[1626321114000],[1626321115000],[1626321116000]]}'""")
                result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}), timeout=10)
                # result = requests.request('POST', url=url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}))
                # result = mureq.post('http://almaty2:8005/action', body=b'{"data":payload}')

                logger.debug("response executed")

                return {
                        "state": 'model successefully executed',
                        "point": point,
                        "start_time": start_time,
                        "finish_time":  str(datetime.datetime.now()),
                        "prediction": result.json().get('prediction'),
                        "anomalies": result.json().get('anomalies'),
                        "model_id": result.json().get('model_id'),
                        "error": False
                        }

            except Exception as exp:
                logger.debug(str(exp))
                return {
                        "state": 'model side caused error}',
                        "point": point,
                        "start_time": start_time,
                        "error_text": str(exp),
                        "error": True
                        }

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
        network = 'operator_default' #service_config.get('network')
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

            container = self.client.containers.run(
                image,
                name=point,
                volumes=[f'{PATH_TO_MLRUNS}:/application/mlruns'], 
                detach=True,
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network=network,
                environment=[
                    f'FEATURES={model_features}', 
                    f'TRACKING_SERVER=http://mlflow:5000', 
                    f'MODEL_ID={model_id}',
                    f'REGRESSORS={regressor_names}',
                    f'PATH_TO_MLRUNS=/application'
                    ],
                command= 'gunicorn -b 0.0.0.0:8005 app:api'
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