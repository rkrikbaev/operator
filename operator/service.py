import datetime
import yaml
import os
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json
from requests import ConnectionError, Timeout, HTTPError

from utils import LOG_LEVEL, TRACKING_SERVER, APP_CODE, MODELS_REG, MODELS_HOST
import utils

logger = utils.get_logger(__name__, loglevel=LOG_LEVEL)


class Service():

    def __init__(self, config):

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
        self.status = "OK"
        self.response = {}
        self.request = {}
        self.host_ip=config.get('host_ip')
        self.container_port=config.get('port')
        self.load_model_flag = config.get('load_model_flag')

        logger.debug(f'Init object complited {self}')

    def run(self, request, model_point):

        def find_model(modelhub_path, exp_id, run_id=None, timestamp = 0, artefact='mlmodel'):

            if run_id is None:

                os.chdir(f'{modelhub_path}/{exp_id}')

                all_folders = [ x for x in os.listdir('.') if os.path.isdir(x) ]

                for folder in all_folders:
                    try:
                        with open(f'{folder}/meta.yaml', 'r') as fl:
                            ts =  yaml.safe_load(fl).get('end_time')
                            if timestamp <= int(ts):
                                timestamp = ts
                                run_id = folder
                            else:
                                print(folder, ts)

                    except FileNotFoundError as exc:
                        print(exc)

                if run_id is None:
                    raise RuntimeError('Din not find any saved model')
                else:
                    print('Variable "run_id" is None latest saved model wil be taken')

            path = f'{modelhub_path}/{exp_id}/{run_id}/{artefact}'

            if os.path.isdir(path):
                return path, exp_id, run_id
            else:
                raise RuntimeError(f'Could not find model by {path}')

        self.request = request

        self.service_name = model_point

        self.response["start_time"] = str(datetime.datetime.now())

        if self.load_model_flag:

                exp_id = model_point
                run_id = self.request.get('model_run_id')

                path, exp_id, run_id = find_model(
                                            '/mlruns',
                                            exp_id,
                                            run_id=run_id
                                        )

                self.request['model_uri'] = path
        else:
            self.request['model_uri'] = None

        self.request = {key: request[key] for key in self.model_keys}

        try:
            container = self.client.containers.get(self.service_name)
            container.remove(force=True)
            logger.debug(f'Delete exist container: {self.service_name}')
            time.sleep(2)
        except NotFound:
            pass

        try:
            model_hub_vol = f'{MODELS_REG}:/mlruns'
            app_code_vol = f'{APP_CODE}/{self.model_type}/app:/app'
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

            self.container_deploy(container_id, counter=0)

            r = self.model_call(self.host_ip, self.host_port)
            self.response.update(r)

            self.container.stop(timeout=10)
#            self.container.remove(force=True)

        except (Exception, RuntimeError) as exc:
            self.status = "internal error"
            logger.debug(exc)

        self.response["service_status"] = self.status
        logger.debug(f'Model {self.service_name} response: {self.response}')

        return self.response

    def container_deploy(self, container_id, counter=0):

        container = self.client.containers.get(container_id)
        status = container.status.lower()

        if status == 'running':

            self.ip_address = container.attrs['NetworkSettings']['Networks'][self.network]['IPAddress']
            self.host_port = container.attrs['NetworkSettings']['Ports'][f'{self.container_port}/tcp'][0]['HostPort']

            assert self.host_port is not None

            logger.debug(f'container started with IP:PORT: {self.ip_address}:{self.host_port}')

        else:

            counter +=1
            time.sleep(1)
            self.container_deploy(container_id, counter)

        if counter > 3:
            self.status = "container deploy error"
            raise RuntimeError('error max tries to get info anbout container')
        
        return

    def model_call(self, host_ip, port, counter=0)->dict:

        response = {}
        logger.debug(self.request)

        try:
            try:
                with requests.Session() as s:
                    url = f'http://{host_ip}:{port}/health'
                    health = s.get(url, timeout=10)
                    logger.debug(f'{url} is {health.ok}')
            except Exception as exc:
                logger.error(exc)
                counter +=1
                time.sleep(5)

                if counter > 3:
                    raise RuntimeError('error max tries to get response from model api')
                else:
                    self.model_call(host_ip, port, counter)
            logger.debug('call predict')
            with requests.Session() as s:
                url = f'http://{host_ip}:{port}/action'
                r = s.post(
                        url,
                        headers={'Content-Type': 'application/json'},
                        data=json.dumps(self.request),
                        timeout=600).json()
            if isinstance(r, dict):
                response.update(r)
            else:
                raise RuntimeError('Unexpected response from model')
        
        except (Exception, RuntimeError) as exc:
            self.status = "model_call error"
            logger.error(exc)

        return response
