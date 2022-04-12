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

#config = load_config(file='app/config/service.yaml')

class Health():

    def __init__(self):
        pass

    def on_get(self, req, resp):

        try:
            print('Health check')
            resp.status = falcon.HTTP_200
            resp.media = {'state': 'ok'}
        except:
            resp.status = falcon.HTTP_500

class Predict():

    def __init__(self):
        #self.service_config = None
        self.feedback = None

    @jsonschema.validate(req_schema=schema.load_schema('request'))
    def on_post(self, req, resp):

        print('POST request received')
        resp.status = falcon.HTTP_400

        data =  req.media

        mtype = data.get('type')
        point = data.get('point')

        payload = {
            'point':point,
            'mtype':mtype,
            'history': data.get('data'),
            'future': data.get('regressor'),
            'features': ["yhat", "yhat_lower", "yhat_upper"],
            'settings':data.get('config')
        }

        service_config = load_config(file='./config/service.yaml')
        print('Loaded config file :', service_config)

        try:

            if service_config:

                print('Call the model type: ', mtype)
                self.service = ModelService(mtype, config=service_config)
                # self.docker = DockerOperator()
                print('Forecast')
                # self.docker.deploy(self.docker_config[mtype], point)
                self.feedback = self.service.call(payload, point)
                # self.docker.remove()
                print('Feedback: ', self.feedback)
#                resp.media = feedback
#                resp.status = falcon.HTTP_201

            else:

                print('Service-config empty!')

        except Exception as error:

            print('Error on POST request')
            logger.error(error)
            resp.status = falcon.HTTP_500

        resp.media = self.feedback

