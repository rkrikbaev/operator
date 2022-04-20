import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument

import time
import requests, json

from middleware.logger import logger

class ModelService():

    """Any HTTP service

    POST http://service_ip:port/action

    request = {
            'settings': settings,
            'history': history,
            'future': future
            'features'[list of fields in response of a model to be included in response of service]
        }

    response = {
            "metadata": {
                "point": "object description from server",
                "start_time": "",
                "finish_time": ""
            },
            "prediction": []
    }

    """

    def __init__(self, mtype, config=None) -> None:

        self.docker_config = config.get('docker')

        if self.docker_config == None:
            logger.warn('Docker config empty')
        self.port = config.get('port')
        if self.port == None:
            logger.warn('PORT must be placed')

        self.point = None
        self.mtype = mtype
        self.container_id = None

    def call(self, payload, point)->dict:

        self.point = point

        container = DockerOperator()
        state = False
        
        state = container.deploy(mtype=self.mtype, port=self.port, container_name=self.point, config=self.docker_config)

        if state:
            url = f'http://{self.point}:{self.port}/action'
            logger.debug(f'url: {url}')
            
            response = {}

            try:

                response = {
                            "state": 'executed',
                            "point": self.point,
                            "start_time": str(datetime.datetime.now())
                            }

                result = requests.request("POST", 
                    url,
                    headers={"Content-Type": "application/json"}, 
                    data=json.dumps(payload))

                response["finish_time"] = str(datetime.datetime.now())
                response['prediction'] = result.json()['yhat']
                
                logger.debug(f'response: {response}')

            except Exception as exp:
                
                response['state'] = 'error'
                response['prediction'] = None
                logger.error(exp)

            container.remove()

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

        self.container_id = None
        self.container = None

    def deploy(self, mtype, port, container_name, config):

        image = config[mtype].get('image')
        if image == None:
            logger.warn('IMAGE must be placed')

        # volumes = config[mtype].get('volumes')
        cpuset_cpus = config[mtype]['limits'].get('cpuset_cpus')
        con_mem_limit = config[mtype]['limits'].get('con_mem_limit')

        count = 0
        containet_state = ' '

        while containet_state != 'exited':
            try:
                # step 1. Create and run container
                if self.container_id == None:

                    self.container_id = self.create(
                        image,
                        ports=port,
                        container_name=container_name,
                        cpuset_cpus=cpuset_cpus,
                        con_mem_limit=con_mem_limit
                        )

                    # self.container_id = self.container.short_id
                    logger.debug(f'step 1: container #{self.container_id} created')

                # step 2. Try to get information about container
                elif containet_state.lower() == 'created':
                    
                    count = count + 1
                    logger.debug(f'attempt #{count} to start container #{self.container_id}')
                    
                    if count > 5:
                        logger.warning('Max retries exeeded')
                        raise DockerException
                    else:
                        self.container = self.client.containers.get(self.container_id) 
                        time.sleep(5)

                # step 2. Return if container was started
                elif containet_state.lower() == 'running':
                    logger.debug(f'step 2: container #{self.container_id} running')
                    return True

                if self.container_id :
                    self.container = self.client.containers.get(self.container_id)
                    containet_state = self.container.attrs['State'].get('Status')
            
            except (APIError, DockerException) as exc:
                logger.error(f'unable to create a docker due to: {exc}')

        else:
            logger.warning(f'container #{self.container_id} exited')
            

    def create(self, image, ports, container_name, cpuset_cpus, con_mem_limit, network='service_network')->str:

        '''Run a docker container using a given image; passing keyword arguments
        documented to be accepted by docker's client.containers.run function
        No extra side effects. Handles and reraises ContainerError, ImageNotFound,
        and APIError exceptions.
        '''

        ports = {ports:None}
        container = None
        volume = [f'{container_name}:/application/mlruns']

        try:

            container = self.client.containers.run(
                image,
                name=container_name,
                ports=ports, 
                volumes=volume, 
                detach=True, 
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network=network
                )

        except ContainerError as exc:
            logger.error("Failed to run container")
            raise exc
        except ImageNotFound as exc:
            logger.error("Failed to find image to run as a docker container")
            raise exc
        except APIError as exc:
            logger.error("Unhandled error")
            raise exc
        except InvalidArgument  as exc:
            logger.error("Docker Invalid Argument")
            raise exc           
        return container.short_id   

    def remove(self):
            
            '''
            Remove a docker container using a given id; passing keyword arguments
            documented to be accepted by docker's client.containers.remove function
            No extra side effects. Handles and reraises APIError exceptions.
            '''
                
            try:               
                container = self.client.containers.get(container_id=self.container_id)
                container.remove(force=True)
                
                logger.debug(f'container {self.container_id} removed')

            except APIError as exc:
                logger.error(f'unable to remove container by APIError error: {exc}')

