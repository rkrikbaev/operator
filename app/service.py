import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')


class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, point, ip_address)->dict:

        tries = 0
        health_ok = False
        logger.debug('Try to call the model first time')

        session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=100,
            pool_maxsize=100)

        session.mount('http://', adapter)
        url = f'http://{ip_address}:8005/health'

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
                
            url = f'http://{ip_address}:8005/action'

            logger.debug(f'query url: {url}')
            logger.debug(f'query payload: {payload}')

            start_time = str(datetime.datetime.now())
            
            try:
                result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps({'data':payload}), timeout=10)

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
    def __init__(self, docker_config, path_to_models):

        self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        self.image = docker_config.get('image')
        self.cpuset_cpus = docker_config['limits'].get('cpuset_cpus')
        self.con_mem_limit = docker_config['limits'].get('con_mem_limit')       
        self.startup = docker_config.get('startup')
        self.model_features = docker_config.get('features')
        self.network = 'operator_default'
        self.path_to_models = path_to_models

    def deploy_container(self, point, model_features, model_id, regressor_names):
        ip_address = None
        
        try:
            container = self.client.containers.get(point)
            container.remove(force=True)
            logger.debug(f'Delete exist container with same point name: {point}')
            time.sleep(2)
        
        except NotFound:
            pass

        logger.debug('Try to create container')
        logger.debug(f'Models config: {point},{model_features},{model_id},{regressor_names}')

        volume_path = f'{self.path_to_models}/mlruns:/application/mlruns'
        logger.debug(f'The conatiner volume path: {volume_path}')          

        container = self.client.containers.run(
                                image=self.image,
                                name=point,
                                volumes=[volume_path], 
                                detach=True,
                                mem_limit=self.con_mem_limit,
                                cpuset_cpus=self.cpuset_cpus,
                                network=self.network,
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

        while wait_counter < self.startup:
            container = self.client.containers.get(container_id)
            container_state = container.status.lower()
                        
            if container_state == ['created']:
                time.sleep(1)
                wait_counter += 1

            if container_state in ['running']:
                ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                logger.debug(f'container #{point}, started with IP: {ip_address}')
                
                return ip_address, container_state                
        else:
            raise RuntimeError(f'Fail to deploy container for point: {point}')