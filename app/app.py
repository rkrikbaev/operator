import falcon
from falcon.media.validators import jsonschema
from celery.result import AsyncResult
import time
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
    def on_post(self, req, resp, task_id):

        logger.debug(f'Request received for task with id: {task_id}')

        if task_id:
            
            task_result = AsyncResult(task_id)
            result = {'status': str(task_result.status), 'result': str(task_result.result)}
            logger.debug(result)

            resp.status = falcon.HTTP_200
            resp.media = result
        
        else:
        
            try:
                file_path = os.path.join(os.getcwd(), 'config/service_config.yaml')
                logger.debug(f'Loaded config file : {file_path}')
                
                with open(file_path, 'r') as fl:
                    service_config =  yaml.safe_load(fl)   

                data =  req.media
                task = predict.delay(config=service_config, data=data) 
                resp.media = {'ts': str(time.ctime()),'task_id': task.id, 'state':'success'}
                logger.debug(f'"ts": {time.ctime()},"task_id": {task.id}, "state":"success"')
            except Exception as exc:
                logger.error(exc)
                resp.status = falcon.HTTP_500
                resp.media = {'ts': str(time.ctime()),'task_id': task.id, 'state':'fail'}
                logger.debug(f'"ts": {time.ctime()},"task_id": {task.id}, "state":"fail"')

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')