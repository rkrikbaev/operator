import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout, HTTPError

from utils import get_logger, LOG_LEVEL, TRACKING_SERVER

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
        self.request = None
        self.host_ip=config.get('host_ip')
        self.container_port=config.get('port')

        logger.debug(f'Init object complited {self}')

    def run(self, request, model_point, model_hub, app_code_path=None):

        logger.debug(f'Deploy object {self}')
        self.service_name = model_point

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

        try:
            model_hub_vol = f'{model_hub}:/mlruns'
            app_code_vol = f'{app_code_path}:/app'
            self.container = self.client.containers.run(
                                image=self.image,
                                name=self.service_name,
                                ports={f'{self.container_port}/tcp': None},
                                volumes=[model_hub_vol, app_code_vol],
                                detach=True,
                                mem_limit=self.con_mem_limit,
                                cpuset_cpus=self.cpuset_cpus,
                                network=self.network,
                                environment=[
                                    f'LOG_LEVEL={LOG_LEVEL}',
                                    f'TRACKING_SERVER={TRACKING_SERVER}',
                                    f'TIMEOUT={self.timeout}'],
                                command=f'gunicorn -b 0.0.0.0:{self.container_port} api:api --timeout 1000 --log-level debug'
                                )
            container_id = self.container.short_id
            logger.debug(f'Created container ID {container_id}')

            if not container_id:
                raise RuntimeError('Created container id cannot be None')

            ip_address, host_port = self.container_deploy(container_id, _counter=0)

            _response = self._model_call(self.host_ip, host_port)
            logger.debug(f'Post request result service._model_call() {_response}')

            self.container.stop(timeout=10)
            self.container.remove(force=True)

            self.response.update(_response)
            self.response["service_status"] = 'OK'

        except Exception as exc:
            logger.debug(exc)

        logger.debug(f'Post request result service._model_call() {self.response}')
        return self.response

    def container_deploy(self, container_id, _counter=0):

        container = self.client.containers.get(container_id)
        _status = container.status.lower()

        if _status == 'running':

             ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
             host_port = container.attrs['NetworkSettings']['Ports'][f'{self.container_port}/tcp'][0]['HostPort']

             assert host_port is not None

             logger.debug(f'container started with IP:PORT: {ip_address}:{host_port}')
             logger.debug(container.attrs['NetworkSettings'])

             return ip_address, host_port

        else:

            _counter +=1
            time.sleep(1)
            self.container_deploy(container_id, _counter)

        if _counter > 3:
            raise RuntimeError('error max tries to get info anbout container')

    def _model_call(self, host_ip, port, _counter=0):

        _response = {"service_status": "error"}

        try:
            with requests.Session() as s:
                url = f'http://{host_ip}:{port}/health'
                logger.debug(url)
                health = s.get(url, timeout=10)
                logger.debug(f'Service API {url} is {health.ok}')
        except Exception as exc:
            logger.error(exc)
            _counter +=1
            time.sleep(5)

            if _counter > 3:
                raise RuntimeError('error max tries to get response from model api')
            else:
                self._model_call(host_ip, port, _counter)
        try:
            with requests.Session() as s:
                url = f'http://{host_ip}:{port}/action'
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
