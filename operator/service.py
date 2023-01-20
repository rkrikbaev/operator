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
        # self.startup = config.get('startup')
        self.network = 'operator_default'
        self.model_type = config.get('type')
        self.timeout = config.get('timeout')

        self.model_keys = ['model_config', 'dataset', 'model_uri', 'metadata', 'period']

        self.service_name = None

        self.response = {}
        self.request = None

        logger.debug(f'Init object complited {self}')
    
    def run(self, name, request):
        
        logger.debug(f'Deploy object {self}')
        self.service_name = name

        self.response["service_status"] = 'error'
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
                                    f'TRACKING_SERVER={TRACKING_SERVER}',
                                    f'TIMEOUT={self.timeout}']
                                ).short_id

            logger.debug(f'Created container ID {container_id}')
            
            if not container_id:
                raise RuntimeError('Created container id cannot be None')
            
            ip_address = self._container_call(container_id, _counter=0)
            
            if not ip_address:
                raise RuntimeError('Service IP cannot be None')
            
            _response = self._model_call(ip_address, _counter=0)
            logger.debug(f'Post request result service._model_call() {_response}')
            
            self.response.update(_response)
            self.response["service_status"] = 'OK'
            
        except Exception as exc:
            logger.debug(exc)
        
        logger.debug(f'Post request result service._model_call() {self.response}')
        return self.response
    
    def _container_call(self, container_id, _counter):
        
        container = self.client.containers.get(container_id)
        _status = container.status.lower()
        
        if _status == 'running':
             ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
             logger.debug(f'container started with IP: {ip_address}')
             return ip_address
        else:
            _counter +=1
            time.sleep(1)
            self._container_call(container_id, _counter)
        
        if _counter > 3:
            raise RuntimeError('error max tries to get info anbout container')

    def _model_call(self, ip_address, _counter):
        
        _response = {"service_status": "error"}
        
        try:  
            with requests.Session() as s:
                url = f'http://{ip_address}:8005/health'
                health = s.get(url, timeout=10)
                logger.debug(f'Service API {url} is {health.ok}')
        except Exception as exc:
            logger.error(exc)
            _counter +=1
            time.sleep(5)
            
            if _counter > 3:
                raise RuntimeError('error max tries to get response from model api')
            else:
                self._model_call(ip_address, _counter)
        try:
            with requests.Session() as s:
                url = f'http://{ip_address}:8005/action'
                r = s.post(
                        url, 
                        headers={'Content-Type': 'application/json'}, 
                        data=json.dumps(self.request), 
                        timeout=600)
                logger.debug(f'{__class__}._model_call() in {__file__} return {r.json()}')
                _response.update(r.json())
                _response["service_status"] = "ok"    
        except Exception as exc:
            logger.error(exc)

        return _response
