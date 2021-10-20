from docker import DockerClient
import os, json
from logger import logger
import requests

from middleware.watchdog import Watchdog

class DockerEngineService(DockerClient):   
     
    def checkout_container_status(self):
        
        resp = self.client.containers.get(self.container_id)
        
        if bool(resp):

            if "Id" in resp.attrs:
                self.container_id = resp.attrs["Id"]
            if "State" in resp.attrs:
                self.container_state = resp.attrs["State"]['Status']
            if bool(resp.ports):
                self.container_port = int(list(resp.ports.values())[0][0]["HostPort"])

        return self.container_id, self.container_state, self.container_port

    def create_container(self, image, port, source, destination):

        print(port)

        resp = self.client.containers.run(
            image=image,
            ports= port,
            volumes={source: {"bind": destination, "mode": "rw"}},
            environment=self.environment,
            detach=True
        )

        if bool(resp):

            if "Id" in resp.attrs:
                self.container_id = resp.attrs["Id"]
            if "State" in resp.attrs:
                self.container_state = resp.attrs["State"]
            if bool(resp.ports):
                self.container_ports = resp.ports

    def stop_container(self):
        self.container = self.client.containers.get(self.container_id)
        self.container.stop()

    def delete_container(self):
        self.container = self.client.containers.get(self.container_id)
        self.container.remove()

    def available_items(self):
        pass

    def update_image(self, repository, tag):
        status = self.client.images.pull(repository=repository, tag=tag, all_tags=False)
        print(status)


class ProphetServiceAPI():

    def __init__(self, service_addr, container_port):

        self.service_addr = service_addr
        self.rest_api_port = container_port

        if (self.service_addr or self.rest_api_port) == None:
            raise ValueError            

    def process_predict_request(self, payload):
        error = {}
        response = None

        response_status_code = 200
        response_text = ''
        try:
            headers = {"Content-Type": "application/json"}
            body = json.dumps(payload)
            url = f"http://{self.service_addr}:{self.rest_api_port}/action"
            response = requests.request("POST", url, headers=headers, data=body)
            logger.debug("get response from the model")
            
            if bool(response):
                response_text = eval(response.text)
                response_status_code = response.status_code
            else:
                response_text = {'state': 'No response from model without errors'}

        except Exception as error:
    
            logger.error(error)
            response_status_code = 500
        
        finally:
            return response_text, response_status_code


class TensorFlowServingService():
    
    def __init__(self, docker_service):
        
        if (self.service_addr or self.rest_api_port) == None:
            raise ValueError

        headers = {"Content-Type": "application/json"}        
        self.service_addr = service_addr
        self.rest_api_port = int(service_port["8005/tcp"][0]["HostPort"])
    
    def predict(self, host, port, model, payload):

        error = {}
        response = {}
        rest_api_port = int(port["8501/tcp"][0]["HostPort"])
        response_text = ''
        
        try:

            body = json.dumps(payload)
            url = f"http://{host}:{rest_api_port}/v1/models/{model}:predict"
            response = requests.request("POST", url, headers=headers, data=body)
            
            response_text = eval(response.text)
            response_status_code = response.status_code
            
            logger.debug(f"get response from the model: {response_text}")

        except Exception as error:

            logger.error(error)
            response_status_code = 500

        finally:

            return response_text, response_status_code