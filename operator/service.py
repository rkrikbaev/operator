import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout, HTTPError

from utils import get_logger, LOG_LEVEL, TRACKING_SERVER, MODELS_REG, APP_CODE

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

        self.network = 'operator_default'
        self.model_type = config.get('type')
        self.timeout = config.get('timeout')

        self.model_keys = ['model_config', 'dataset', 'model_uri', 'metadata', 'period']

        self.service_name = None

        self.response = {}
        self.container_port=8007

        self.container_id = None

        logger.debug(f'Init object complited {self}')

    def run(self, service_name, request):

        self.service_name = service_name

        try:
            container = self.client.containers.get(self.service_name)
            container.remove(force=True)
            logger.debug(f'Delete exist container: {self.service_name}')
            time.sleep(2)
        except NotFound:
            pass

        self.response["service_status"] = 'error'
        self.response["start_time"] = str(datetime.datetime.now())

        data = {key: request[key] for key in self.model_keys}
        logger.debug(f'Filter request fields: {list(data.keys())}')

        response = self.model_call(self.container_deploy(), data)
        logger.debug(f'Post request result service._model_call() {response}')
            
        self.response.update(response)
        self.response["service_status"] = 'OK'
            
        logger.debug(f'Post request result service._model_call() {self.response}')
        return self.response
    
    def container_deploy(self, container_id=None, counter=0):

        self.container_id = container_id

        saved_models = f'{MODELS_REG}:/mlruns'
        app_code = f'{APP_CODE}/{self.model_type}/app:/app'
        
        if self.container_id is None:
            try:
                self.container = self.client.containers.run(
                                    image=self.image,
                                    name=self.service_name,
                                    volumes=[saved_models, app_code],
                                    ports={f'{self.container_port}/tcp': None},
                                    detach=True,
                                    mem_limit=self.con_mem_limit,
                                    cpuset_cpus=self.cpuset_cpus,
                                    network=self.network,
                                    environment=[
                                        f'LOG_LEVEL={LOG_LEVEL}', 
                                        f'TRACKING_SERVER={TRACKING_SERVER}',
                                        f'TIMEOUT={self.timeout}'],
                                    command=f'gunicorn -b 0.0.0.0:{self.port} api:api --timeout 1000 --log-level debug'
                                    )
                
                self.container_id = self.container.short_id
                self.container_deploy(self, self.container_id)

            except Exception as exc:
                logger.debug(exc)
        else:

            self.container.reload()
            self.status = self.container.status.lower()

            if self.status == 'running':
             
                container_ip_address = self.container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
                host_port = self.container.attrs['NetworkSettings']['Ports'][f'{self.container_port}/tcp'][0]['HostPort']
                
                logger.debug(f'container started with IP:PORT: {container_ip_address}:{host_port}')
                
                return container_ip_address, host_port
            
            else:

                counter +=1
                time.sleep(1)
                self.container_deploy(self.container_id, counter)
        
                if counter > 3:
                    raise RuntimeError('error max tries to get info anbout container')

    def model_call(self, ip_address, host_port, data):
        
        response = {"service_status": "error"}
        
        try:  
            with requests.Session() as s:
                url = f'http://{ip_address}:{host_port}/health'
                health = s.get(url, timeout=10)
                logger.debug(f'Service API {url} is {health.ok}')
        except Exception as exc:
            logger.error(exc)
            counter +=1
            time.sleep(5)
            
            if counter > 3:
                raise RuntimeError('error max tries to get response from model api')
            else:
                self.model_call(ip_address, counter)
        try:
            with requests.Session() as s:
                url = f'http://{ip_address}:{host_port}/action'
                r = s.post(
                        url, 
                        headers={'Content-Type': 'application/json'}, 
                        data=json.dumps(data), 
                        timeout=600)
                logger.debug(f'{__class__}._model_call() in {__file__} return {r.json()}')
                response.update(r.json())
                response["service_status"] = "ok" 
                self.container.stop()
                self.container.remove()   
        except Exception as exc:
            logger.error(exc)

        return response
