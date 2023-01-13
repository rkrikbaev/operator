import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout
    
from utils import get_logger, LOG_LEVEL, BASE_PATH, TRACKING_SERVER

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

    def deploy(self, name):

        logger.debug(f'Deploy object {self}')
        
        self.service_name = name
        try:
            container = self.client.containers.get(self.service_name)
            container.remove(force=True)
            logger.debug(f'Delete exist container: {self.service_name}')
            time.sleep(2)
        except NotFound:
            pass

        volume_app = f'{BASE_PATH}/mlservices/{self.model_type}:/application'
        try:
            container_id = self.client.containers.run(
                                image=self.image,
                                name=self.service_name,
                                volumes=[volume_app], 
                                detach=True,
                                mem_limit=self.con_mem_limit,
                                cpuset_cpus=self.cpuset_cpus,
                                network=self.network,
                                environment=[
                                    f'LOG_LEVEL={LOG_LEVEL}', 
                                    f'TRACKING_SERVER={TRACKING_SERVER}'],
                                command='gunicorn -b 0.0.0.0:8005 api:api'
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

            if self.container_state  in ['running']:
                self.ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                logger.debug(f'container #{self.service_name}, started with IP: {self.ip_address}')
                
                return self.ip_address, self.container_state                 
        else:
            raise RuntimeError(f'Fail to deploy container for point: {self.service_name}')

    def call(self, *args):

        self.payload = args[0]
        start_time = None
        health_ok = False

        session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=100,
            pool_maxsize=100)

        session.mount('http://', adapter)
        url = f'http://{self.ip_address}:8005/health'

        tries = 0
        while tries < 5:
          
            try:
                health = session.get(url, timeout=600)
                health_ok = health.ok

                url = f'http://{self.ip_address}:8005/action'

                logger.debug(f'query url: {url}')
                logger.debug(f'query payload: {self.payload.keys}')

                start_time = str(datetime.datetime.now())
                
                result = requests.post(url, headers={'Content-Type': 'application/json'}, data=json.dumps(self.payload), timeout=10)
                logger.debug(result)

                return {
                        "point": self.service_name,
                        "start_time": start_time,
                        "finish_time": str(datetime.datetime.now()),
                        "prediction": result.json().get('prediction'),
                        "anomalies": result.json().get('anomalies'),
                        "model_uri": result.json().get('model_uri'),
                        }

            except (ConnectionError, Timeout):
                tries += 1
                logger.debug(f'query /health success: {health_ok}: tries: {tries}: sleep: {tries*tries} sec')
                time.sleep(tries)

        else:
            logger.debug(f' Tried to call a model for point {self.service_name} {tries} times')
            return {
                    "state": 'model side caused error',
                    "point": self.service_name,
                    "start_time": start_time,
                    "error_text": "ConnectionError"
                    }