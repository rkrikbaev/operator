import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound
import time, os
import requests, json

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

class ModelAsHTTPService():

    def __init__(self) -> None:
        pass

    def call(self, payload, point, port)->dict:

        # time delay to deploy application in the container
        time.sleep(2)

        url = f'http://{point}:{port}/action'
        logger.debug(f'query url: {url}')

        response = {
                    "state": 'executed',
                    "point": point,
                    "start_time": str(datetime.datetime.now())
                    }
        try:

            result = requests.request(
                'POST', 
                url,
                headers={'Content-Type': 'application/json'}, 
                data=json.dumps(payload))

            response["finish_time"] = str(datetime.datetime.now())
            response['predictions'] = result.json().get('yhat')
            logger.debug(f'response: {response}')
        
        except Exception as exp:
            response['state'] = 'error'
            logger.error(exp)

        return response 

class DockerOperator():
    """
    Class to work with docker objects
    """
    def __init__(self,):

        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        except DockerException as exc:
            logger.error(f'Connection with docker.socket aborted {exc}')
            raise exc

    def deploy(self, port, point, config):

        image = config.get('image')
        cpuset_cpus = config['limits'].get('cpuset_cpus')
        con_mem_limit = config['limits'].get('con_mem_limit')
            
        try:
            container = self.client.containers.get(point)
            container.remove(force=True)
        except NotFound:
            container = self.client.containers.run(
                image,
                name=point,
                ports={port:port}, 
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
                    # service_ip = container.attrs['NetworkSettings']['Networks']['service_network']['IPAddress']
                    logger.debug(f'container #{container_id}, state: {container_state} with name: {point}')
                    break                
                else:
                    raise RuntimeError(f'Fail to deploy container for point {point}')
            
            return {'id':container_id, 'service_ip':point, 'state': container_state}

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