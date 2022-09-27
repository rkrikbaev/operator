import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time, os
import requests, json
from requests import ConnectionError

from helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')


class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, model_point, ip_address)->dict:
        health_ok = False
        logger.debug('Try to call the model first time')

        session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=100,
            pool_maxsize=100)

        session.mount('http://', adapter)
        url = f'http://{ip_address}:8005/health'

        tries = 0
        while tries < 5:
          
            try:
                health = session.get(url, timeout=600)
                health_ok = health.ok
                logger.debug(f'query url: {url} status: {health_ok}')

                url = f'http://{ip_address}:8005/action'

                logger.debug(f'query url: {url}')
                logger.debug(f'query payload: {payload}')

                start_time = str(datetime.datetime.now())
                
                try:
                    result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps(payload), timeout=10)

                    return {
                            "point": model_point,
                            "start_time": start_time,
                            "finish_time":  str(datetime.datetime.now()),
                            "prediction": result.json().get('prediction'),
                            "anomalies": result.json().get('anomalies'),
                            "model_uri": result.json().get('model_uri'),
                            }

                except Exception as exp:
                    logger.debug(str(exp))
                    
                    return {
                            "state": 'model side caused error}',
                            "point": model_point,
                            "start_time": start_time,
                            "error_text": str(exp)
                            }

            except ConnectionError as exc:
                logger.debug(f'query /health fail by: {exc}')
                tries += 1
                logger.debug(f'query /health success: {health_ok}: tries: {tries}: sleep: {tries*tries} sec')
                time.sleep(tries)

        else:
            logger.debug(f' Tried to call a model for point {model_point} {tries} times')


class DockerController():
    """
    Class to work with docker objects
    """
    def __init__(self, docker_config, path_to_models, path_to_code):

        self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        self.image = docker_config.get('image')
        self.cpuset_cpus = docker_config['limits'].get('cpuset_cpus')
        self.con_mem_limit = docker_config['limits'].get('con_mem_limit')       
        self.startup = docker_config.get('startup')
        self.network = 'operator_default'
        self.path_to_models = path_to_models
        self.path_to_code = path_to_code

    def deploy_container(self, point):
        ip_address = None
        
        try:
            container = self.client.containers.get(point)
            container.remove(force=True)
            logger.debug(f'Delete exist container with same point name: {point}')
            time.sleep(2)
        
        except NotFound:
            pass

        volume_mlruns = f'{self.path_to_models}/mlruns:/application/mlruns'         
        volume_app = f'{self.path_to_code}:/application' 

        container = self.client.containers.run(
                                image=self.image,
                                name=point,
                                volumes=[volume_mlruns, volume_app], 
                                detach=True,
                                mem_limit=self.con_mem_limit,
                                cpuset_cpus=self.cpuset_cpus,
                                network=self.network,
                                environment=[
                                    f'TRACKING_SERVER=http://mlflow:5000', 
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