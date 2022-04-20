import falcon
from falcon.media.validators import jsonschema
from services import ModelService

from middleware.logger import logger

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

        logger.debug('POST request received')

        data =  req.media

        mtype = data.get('type').lower()
        point = data.get('point').lower()

        payload = {
            'history': data.get('data'),
            'future': data.get('regressor'),
            'features': ["yhat", "yhat_lower", "yhat_upper"],
            'settings': data.get('config')
        }

        service_config = load_config(file='./app/config/service.yaml')
        logger.debug(f'Loaded config file : {service_config}')

        if service_config:
            self.service = ModelService(mtype, config=service_config)
        else:
            logger.warn('Service-config empty!')
        try:
                self.feedback = self.service.call(payload, point)
                logger.debug(f'feedback on request: {self.feedback}')
                resp.status = falcon.HTTP_201
        except Exception as error:
            logger.error(error)
            resp.status = error
        
        resp.media = self.feedback

