import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time
import requests, json

import logging, sys
logging.basicConfig(stream=sys.stdout, level=logging.DEBUG,
                    format=f"%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s")
logger = logging.getLogger(__name__)


class ModelService():

    def __init__(self, mtype, config=None) -> None:

        self.docker_config = config.get('docker')
        self.port = config.get('port')
        self.mtype = mtype

    def call(self, payload, point)->dict:

        container = DockerOperator()
        container_id, container_ip = container.deploy(mtype=self.mtype, port=self.port, point=point, config=self.docker_config)

        # time delay to deploy application in the container
        time.sleep(2)
        if container_id:

            url = f'http://{container_ip}:{self.port}/action'
            logger.debug(f'query url: {url}')

            response = {
                        "state": 'executed',
                        "point": point,
                        "start_time": str(datetime.datetime.now())
                        }
            try:

                result = requests.request(
                    "POST", 
                    url,
                    headers={"Content-Type": "application/json"}, 
                    data=json.dumps(payload))

                response["finish_time"] = str(datetime.datetime.now())
                response['predictions'] = result.json().get('yhat')
                logger.debug(f'response: {response}')
            
            except Exception as exp:
                response['state'] = 'error'
                logger.error(exp)
            
            container.remove(container_id)
            return response 

class DockerOperator():
    """
    Class to work with docker objects
    """
    def __init__(self):

        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        except DockerException as exc:
            logger.error(f'Connection with docker.socket aborted {exc}')
            raise exc

    def deploy(self, mtype, port, point, config):

        image = config[mtype].get('image')
        cpuset_cpus = config[mtype]['limits'].get('cpuset_cpus')
        con_mem_limit = config[mtype]['limits'].get('con_mem_limit')
            
        try:
            container = self.client.containers.get(point)
            container.remove(force=True)
        except NotFound:
            container = self.client.containers.run(
                image,
                name=point,
                ports={port:None}, 
                volumes=[f'{point}:/application/mlruns'], 
                detach=True, 
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network='service_network'
                )
            container_id = container.short_id
            container_state = container.status.lower()   
            
            logger.debug(f'container #{container_id} {container_state}')
            
            time.sleep(2)
            wait_counter = 0

            while wait_counter < 2:
                container = self.client.containers.get(container_id)
                container_state = container.attrs['State'].get('Status').lower()           
                if container_state == ['created']:
                    time.sleep(1)
                    wait_counter += 1
                elif container_state in ['running']:
                    container_ip = container.attrs['NetworkSettings']['Networks']['service_network']['IPAddress']
                    logger.debug(f'container #{container_id} {container_state}')
                    break                
                else:
                    raise RuntimeError(f'Fail to deploy container for point {point}')
            
            return container_id, container_ip

        except (APIError, DockerException, RuntimeError) as exc:
            logger.error(f'Docker API error: {exc}')
            logger.error('Fail to create container')


    def remove(self, container_id):
            
        '''
        Remove a docker container using a given id; passing keyword arguments
        documented to be accepted by docker's client.containers.remove function
        No extra side effects. Handles and reraises APIError exceptions.
        '''
            
        try:               
            container = self.client.containers.get(container_id)
            container.remove(force=True)
            logger.debug(f'container {container.short_id} removed')
        except APIError as exc:
            logger.error(f'unable to remove container by APIError error: {exc}')