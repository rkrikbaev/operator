import falcon
from falcon.media.validators import jsonschema
from services import ModelService, DockerOperator
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# from schemas import *
from schemas import schema


PATH_TO_MODELS = '/tmp/models/'
PATH_TO_SOURCE = '/tmp/source'

from middleware.helper import load_config

config = load_config(file='app/config/service.yaml')

class Health():
    
    def __init__(self):
        pass

    def on_get(self, req, resp):

        try:
            resp.status = falcon.HTTP_200
            resp.media = {'state': 'ok'}
        except:
            resp.status = falcon.HTTP_500

class Predict():

    def __init__(self):
        self.service_config = None

    @jsonschema.validate(req_schema=schema.load_schema('request'))
    def on_post(self, req, resp):

        resp.status = falcon.HTTP_400

        data =  req.media

        mtype = data['metadata'].get('type')
        point = data['metadata'].get('point')

        payload = {
            'history': data.get('history'), 
            'fututre': data.get('fututre'), 
            'features': data.get('features'),
            'config':data.get('config')}

        self.service_config = load_config(file='app/config/service.yaml')

        try:
            if self.service_config:
                self.service = ModelService(mtype, config=self.service_config)
                # self.docker = DockerOperator()

                # self.docker.deploy(self.docker_config[mtype], point) 
                feedback = self.service.call(payload, point, config=self.service_config)
                # self.docker.remove()
                
                resp.media = feedback
                resp.status = falcon.HTTP_201
            
        except Exception as error:
            logger.error(error)
            resp.status = falcon.HTTP_500