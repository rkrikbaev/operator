import datetime

from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound, InvalidArgument, NotFound

import time
import requests, json

from middleware.logger import logger

class ModelService():

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
        
        # time delay to deploy application in the container
        time.sleep(2)
        
        if state:

            url = f'http://{self.point}:{self.port}/action'
            logger.debug(f'query url: {url}')
            
            response = {
                        "state": 'executed',
                        "point": self.point,
                        "start_time": str(datetime.datetime.now())
                        }
            try:

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

    def deploy(self, mtype, port, container_name, config):

        image = config[mtype].get('image')
        
        if image == None:
            logger.warn('IMAGE must be placed')

        # volumes = config[mtype].get('volumes')
        cpuset_cpus = config[mtype]['limits'].get('cpuset_cpus')
        con_mem_limit = config[mtype]['limits'].get('con_mem_limit')

        watchdog = False

        while True:
            
            try:
                
                if self.container_id:

                    self.container = self.client.containers.get(self.container_id)
                    
                    if self.container:

                        container_state = self.container.attrs['State'].get('Status')

                        if watchdog:
                            raise RuntimeError('Fail to deploy container') 

                        elif container_state.lower() == 'created':
                            time.sleep(5)
                            watchdog = True

                        elif container_state.lower() in ['exited', 'running']:
                            logger.debug(f'container #{self.container_id} {container_state.lower()}')
                            return True

                    else:
                        raise RuntimeError('fail to get container object by ID')
                
                else:

                    try:
                        
                        self.client.containers.get(container_name).remove(force=True)
                    
                    except NotFound as exc:
                    
                        self.container_id = self.create(
                            image,
                            ports=port,
                            container_name=container_name,
                            cpuset_cpus=cpuset_cpus,
                            con_mem_limit=con_mem_limit
                            )

                        logger.debug(f'container #{self.container_id} created')

                    time.sleep(5)
            
            except (APIError, DockerException, RuntimeError) as exc:
                logger.error(f'Docker API error: {exc}')
            

    def create(self, image, ports, container_name, cpuset_cpus, con_mem_limit, network='service_network')->str:

        '''Run a docker container using a given image; passing keyword arguments
        documented to be accepted by docker's client.containers.run function
        No extra side effects. Handles and reraises ContainerError, ImageNotFound,
        and APIError exceptions.
        '''

        try:

            container = self.client.containers.run(
                image,
                name=container_name,
                ports={ports:None}, 
                volumes=[f'{container_name}:/application/mlruns'], 
                detach=True, 
                mem_limit=con_mem_limit,
                cpuset_cpus=cpuset_cpus,
                network=network
                )

            return container.short_id
       
        except Exception as exc:
            logger.error(exc)  


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