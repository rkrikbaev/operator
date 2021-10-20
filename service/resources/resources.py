import falcon
from falcon.media.validators import jsonschema

from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError

import time

from config.logger import logger
from schemas import load_schema
from config.storage import storage
from app import Service

# Make prediction
class Prediction():

    def __init__(self, config):

        try:
            self.service = Service(config)
        except (APIError, DockerException) as exc:
            logger.error(f'Cannot connect to Docker API due to {0}', exc)
        
    @jsonschema.validate(req_schema=load_schema('request'))
    def on_post(self, req, resp):

        resp.status = falcon.HTTP_400
        data =  req.media
    
        try:
            resp.media = self.service.mainFlow(data)
            resp.status = falcon.HTTP_201
        except Exception as error:
            logger.error(error)
            resp.status = falcon.HTTP_500  
