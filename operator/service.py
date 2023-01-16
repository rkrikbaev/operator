import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout, HTTPError
    
from utils import get_logger, LOG_LEVEL, BASE_PATH, TRACKING_SERVER, MODELS_REG, APP_CODE

logger = get_logger(__name__, loglevel=LOG_LEVEL)


class Service():

    def __init__(self, config):

        logger.debug(f'Read service config file {config}')
        
        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
            logger.debug(f'Create docker client object {self.client}')
        except Exception as exc:
            logger.error(exc)

        self.image = config.get('image')
        self.cpuset_cpus = config.get('limits').get('cpuset_cpus')
        self.con_mem_limit = config.get('limits').get('con_mem_limit')       
        self.startup = config.get('startup')
        self.network = 'operator_default'
        self.model_type = config.get('type')

        self.model_keys = ['model_config', 'dataset', 'model_uri', 'metadata', 'period']

        self.service_name = None

        self.response = {}
        self.request = None

        logger.debug(f'Init object complited {self}')
    
    def run(self, name, request):
        
        logger.debug(f'Deploy object {self}')
        self.service_name = name

        self.response["service_state"] = 'error'
        self.response["start_time"] = str(datetime.datetime.now())           

        self.request = {key: request[key] for key in self.model_keys}
        logger.debug(f'Filter request fields: {list(self.request.keys())}')

        try:
            container = self.client.containers.get(self.service_name)
            container.remove(force=True)
            logger.debug(f'Delete exist container: {self.service_name}')
            time.sleep(2)
        except NotFound:
            pass

        saved_models = f'{MODELS_REG}:/opt/mlruns'
        app_code = f'{APP_CODE}/{self.model_type}:/application'

        logger.debug(f'{saved_models}')
        logger.debug(f'{app_code}')

        try:
            container_id = self.client.containers.run(
                                image=self.image,
                                name=self.service_name,
                                volumes=[saved_models, app_code],
                                detach=True,
                                mem_limit=self.con_mem_limit,
                                cpuset_cpus=self.cpuset_cpus,
                                network=self.network,
                                environment=[
                                    f'LOG_LEVEL={LOG_LEVEL}', 
                                    f'TRACKING_SERVER={TRACKING_SERVER}']
                                ).short_id
            logger.debug(f'Created container ID {container_id}')
            
            if not container_id:
                raise RuntimeError('Created container id cannot be empty')
            
            # try to call deployed service
            wait_counter = 0
            while True:
                    container = self.client.containers.get(container_id)
                    container_state = container.status.lower()

                    logger.debug(f'Upload state of container object by ID: {container_id}')
                    logger.debug(f'State of container: {container_state}')
                                
                    if container_state == 'created':
                        logger.debug(f'Container created and waiting for start-up: {container_id}')
                        wait_counter += 1
                        
                        if wait_counter >= 3:
                            raise RuntimeError('Cannot start service with model')
                        else:
                            time.sleep(1)

                    if container_state == 'running':
                        time.sleep(2)
                        logger.debug(f'Container running: {container_id}')
                        ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                        
                        # try to get health response from the service
                        url = f'http://{ip_address}:8005/health'
                        health = requests.get(url, timeout=1)
                        if health.ok:
                            logger.debug(f'Model API health is ok: {health.status_code}')
                            url = f'http://{ip_address}:8005/action'
                            r = requests.post(
                                url, 
                                headers={'Content-Type': 'application/json'}, 
                                data=json.dumps(self.request), 
                                timeout=120)
                            self.response.update(r.json())
                            self.response["service_state"] = str(r.status_code)
                            self.response.update(self._call(ip_address, payload=self.request))
                            self.response["finish_time"] = str(datetime.datetime.now())                  
                            break
        
        except Exception as exc:
            logger.error(exc)

        return self.response