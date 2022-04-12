import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound

import os
import time
import requests, json

import logging, os

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


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

        self.host = self.point = None
        self.mtype = mtype
        self.container_id = None

    def call(self, payload, point)->dict:

        self.point = point

        container = DockerOperator()
        print('Create service')
        container.deploy(mtype=self.mtype, port=self.port, container_name=self.point, config=self.docker_config)

        url = f'http://{self.point}:{self.port}/action' # point will be added to answer from url
        print('url',url)
        start = str(datetime.datetime.now())

        response = {'state': 'error'}

        try:

            result = requests.request("POST", 
                url,
                headers={"Content-Type": "application/json"}, 
                data=json.dumps(payload))
            #result = eval(response
            #print('result', result)

            stop = str(datetime.datetime.now())

            response = {
                        "state": 'ok',
                        "metadata": {
                            "point": self.point,
                            "start_time": start,
                            "finish_time": stop
                            },
                        'prediction': result.json()
            }

            print('Response: ', result.json())
            print('Request: ', payload)

        except Exception as exp:

            logger.error(exp)
           # container.remove()

        container.remove()

        return response # if error {'state': 'error'} else dict

class DockerOperator():
    """
    Class to work with docker objects
    """
    def __init__(self):
        # pass
        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        except DockerException as exc:
            logger.error(f'Connection with docker.socket aborted {exc}')
            raise exc

    def deploy(self, mtype, port, container_name, config):

        image = config[mtype].get('image')
        if image == None:
            logger.warn('IMAGE must be placed')

        # volumes = config[mtype].get('volumes')
        cpuset_cpus = config[mtype]['limits'].get('cpuset_cpus')
        con_mem_limit = config[mtype]['limits'].get('con_mem_limit')

        self.container_id = None
        count = 0
        containet_state = 'stop'

        try:

            while containet_state != 'exited':

                # step 1. Create and run container
                if self.container_id == None:

                    self.container = self.create(
                        image,
                        ports=port,
                        container_name=container_name,
                        cpuset_cpus=cpuset_cpus,
                        con_mem_limit=con_mem_limit
                        )

                    self.container_id = self.container.short_id
                    print('step 1', self.container_id)

                # step 3. Try to get information about container
                elif containet_state.lower() == 'created':
                    print('step 2',self.container_id)
                    time.sleep(5)
                    count = count + 1
                    self.container = self.client.containers.get(self.container_id)

                # step 2. Return if container was started
                elif containet_state.lower() == 'running':
                    print('step 3',self.container_id)
                    return self.container.short_id

                elif count >= 5:
                    logger.warning('Max retries exeeded')
                    break

                containet_state = self.container.attrs['State'].get('Status')

            else:
                logger.warning(f'Container {self.container_id} failed to start')

        except (APIError, DockerException) as exc:

            logger.error(f'Error create docker: {exc}')
            raise exc

    def create(self, image, ports, container_name, cpuset_cpus, con_mem_limit):

        '''Run a docker container using a given image; passing keyword arguments
        documented to be accepted by docker's client.containers.run function
        No extra side effects. Handles and reraises ContainerError, ImageNotFound,
        and APIError exceptions.
        '''

        network = os.getenv('SERVICES_NETWORK', default='service_network')

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

            if "Name" in container.attrs.keys():
                logger.info(f'Container {container.attrs["Name"]} is now running.')

        except ContainerError as exc:
            logger.error("Failed to run container")
            raise exc
        except ImageNotFound as exc:
            logger.error("Failed to find image to run as a docker container")
            raise exc
        except APIError as exc:
            logger.error("Unhandled error")
            raise exc

        return container   

    def remove(self):
            
            '''
            Remove a docker container using a given id; passing keyword arguments
            documented to be accepted by docker's client.containers.remove function
            No extra side effects. Handles and reraises APIError exceptions.
            '''
                
            try:               
                print('try to remove container')
                container = self.client.containers.get(container_id=self.container_id)
                container.remove(force=True)
                
                logger.info(f'Container {self.container_id} was removed')
                self.container_id = None

            except APIError as exc:
                logger.error(f'Unhandled APIError error: {exc}')
                raise exc

