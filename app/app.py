import falcon
from falcon.media.validators import jsonschema
from celery.result import AsyncResult
import time
from requests import request
import yaml
import os

from schemas import schema
from tasks import predict

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')


class Health():

    def __init__(self):
        pass

    def on_get(self, req, resp):

        try:
            logger.debug('Health check')
            resp.status = falcon.HTTP_200
            resp.media = {'state': 'ok'}
        except:
            resp.status = falcon.HTTP_500

class Predict():

    def __init__(self):
        pass

    @jsonschema.validate(req_schema=schema.load_schema('request'))
    def on_post(self, req, resp):

        request = req.media
        task_id = request.get('uid')
        mtype = request.get('type').lower()
        point = request.get('point').lower()
        model_features = request.get('features')
        regressor_names = request.get('regressor_names')
        model_id = request.get('model_id')
        payload = request.get('payload')
    
        file_path = os.path.join(os.getcwd(), 'service_config.yaml')
        with open(file_path, 'r') as fl:
            f =  yaml.safe_load(fl)
            service_config = f.get('docker')[mtype]

        if task_id or len(task_id)==0:
            
            task_result = AsyncResult(task_id)
            result = {'status': str(task_result.status), 'result': str(task_result.result)}
            logger.debug(result)

            resp.status = falcon.HTTP_200
            resp.media = result
        
        else:

            try:

                task = predict.delay(service_config, payload, point, model_id, model_features, regressor_names) 
                resp.media = {'ts': str(time.ctime()),'task_id': task.id, 'state':'success'}
                logger.debug(f'"ts": {time.ctime()},"task_id": {task.id}, "state":"success"')
            
            except Exception as exc:
                logger.error(exc)
                resp.status = falcon.HTTP_500
                resp.media = {'state':'fail', 'error':exc}

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')