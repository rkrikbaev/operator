import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout
    
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

        self.ip_address = None
        self.service_name = None

        self.response = None
        self.request = None

        logger.debug(f'Init object complited {self}')
    
    def run(self, name, request):

        logger.debug(f'Deploy object {self}')
        self.service_name = name

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
        except Exception as exc:
            logger.error(exc)
        
        wait_counter = 0
        while wait_counter < 3:

            container = self.client.containers.get(container_id)
            container_state = container.status.lower()
            logger.debug(f'Upload state of container object by ID: {container_id}')
            logger.debug(f'State of container: {container_state}')
                        
            if container_state == 'created':
                logger.debug(f'Container created and waiting for start-up: {container_id}')
                wait_counter += 1

            if container_state == 'running':
                time.sleep(2)
                logger.debug(f'Container running: {container_id}')
                self.ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                
                try:
                    self.response = self.call(self.request)
                    logger.debug(f'Got response from model {self.response}')
                except Exception as exc:
                    logger.error(exc)
                finally:
                    break

            time.sleep(1)

        return self.response

    def call(self, *args):

        self.payload = args[0]
        health_ok = False

        response =  {
            "error_state": 'ok',
            "point": self.service_name,
            "start_time": str(datetime.datetime.now()),
            "finish_time": None,
            "prediction": None,
            "anomalies": None,
            "model_uri": None                 
            }

        session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=100,
            pool_maxsize=100)

        session.mount('http://', adapter)
        url = f'http://{self.ip_address}:8005/health'
        result = None

        while True:
          
            try:
                health = session.get(url, timeout=600)
                logger.debug(f'Model API health status: {health}')
                break

            except Exception as exc:
                response["error_state"] = f'Model API health is {health}'
                response["finish_time"] = str(datetime.datetime.now())
                logger.error(exc)

        url = f'http://{self.ip_address}:8005/action'

        logger.debug(f'query url: {url}')
        logger.debug(f'query payload: {list(self.payload.keys())}')
        
        try:
            result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps(self.payload), timeout=10).json()
            response["finish_time"] = str(datetime.datetime.now())
            response.update(result)
            logger.debug(f'Response from model: {response}')  
        except Exception as exc:
            response["error_state"] = f'Error when call predict'
            logger.error(f'Try call the model: {exc}')
        return response