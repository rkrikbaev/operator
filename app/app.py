import falcon
from falcon.media.validators import jsonschema
from celery.result import AsyncResult
import time

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
        task_id = request.get('task_id')

        if task_id and len(task_id)>10:
            logger.debug(f'Request result from celery: {task_id}')

            try:
                task_result = AsyncResult(task_id)
                result = {'status': str(task_result.status), 'result': str(task_result.result), 'state': 'success'}
                resp.status = falcon.HTTP_200
                resp.media = result

            except Exception as err:
                logger.debug(f'Broker call has exception: {err}')
                resp.status = falcon.HTTP_500
                resp.media = {'state': 'error', 'error_text': str(err)}
    
        else:

            try:
                task = predict.delay(request)
                resp.media = {'ts': str(time.ctime()),'task_id': task.id, 'state':'success'}
                logger.debug(f'"ts": {time.ctime()},"task_id": {task.id}, "state":"success"')
            
            except Exception as exc:
                logger.debug(exc)
                resp.status = falcon.HTTP_500
                resp.media = {'state':'error', 'error_text': str(exc)}

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')