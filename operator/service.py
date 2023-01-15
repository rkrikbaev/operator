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

        self.ip_address = None
        self.service_name = None

        logger.debug(f'Init object complited {self}')

    def run(self, name, request):

        logger.debug(f'Deploy object {self}')
        self.service_name = name
        self.request = request

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
        except Exception as exc:
            logger.debug(exc)
        wait_counter = 0

        while wait_counter < self.startup:

            container = self.client.containers.get(container_id)
            self.container_state = container.status.lower()
                        
            if self.container_state  == ['created']:
                time.sleep(1)
                wait_counter += 1

            if self.container_state  == ['running']:

                self.ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                logger.debug(f'container #{self.service_name}, started with IP: {self.ip_address}')

                self.response = self.call(self.request)
                
                return self.response                 
        else:
            raise RuntimeError(f'Fail to deploy container for point: {self.service_name}')

    def call(self, *args):

        self.payload = args[0]
        health_ok = False

        response =  {
            "state": '200',
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
        tries = 0

        while tries < 5:
          
            try:
                health = session.get(url, timeout=600)
                health_ok = health.ok
                return

            except Exception as exc:
                tries += 1
                logger.error(exc)
                logger.debug(f'query /health success: {health_ok}: tries: {tries}: sleep: {tries*tries} sec')
                time.sleep(tries)
        else:        
            response["state"] = '500'
            response["finish_time"] = str(datetime.datetime.now())

        url = f'http://{self.ip_address}:8005/action'

        logger.debug(f'query url: {url}')
        logger.debug(f'query payload: {list(self.payload.keys())}')
        
        try:
            result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps(self.payload), timeout=10).json()
            response["finish_time"] = str(datetime.datetime.now())
            response.update(result)
            logger.debug(f'operarator@service: response from model: {response}')  
        except Exception as exc:
            response["state"] = '500'
            logger.error(f'Try call the model: {exc}')
        return response