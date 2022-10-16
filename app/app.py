import falcon
from celery.result import AsyncResult
import time

from tasks import predict
from helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')


class Health():

    def __init__(self):
        pass

    def on_get(self, req, resp):

        logger.debug('Health check')
        resp.status = falcon.HTTP_200
        resp.media = {'state': 'ok'}


class Predict():

    def __init__(self):
        pass

    def on_post(self, req, resp):

        request = req.media
        task_id = request.get('task_id')
        _time = int(time.time())

        resp.status = falcon.HTTP_200
        
        if task_id and len(task_id)>10:
            logger.debug(f'Request result from celery: {task_id}')

            try:
                task = AsyncResult(task_id)
                result = task.result
                status = task.status

                resp.media = {
                    'time_exec': _time,
                    'task_status': status, 
                    'task_id': task_id,
                    'result':result
                    }

            except Exception as err:
                logger.debug(f'Broker call has exception: {err}')
                resp.status = falcon.HTTP_500
    
        elif task_id is None:

            try:
                task = predict.delay(request)

                resp.media = {
                    'time_exec': _time,
                    'task_id': task.id,
                    'task_status': "DEPLOYED",
                    'result': None
                    }
            
            except Exception as err:
                logger.debug(err)
                resp.status = falcon.HTTP_500
        
        else:
                resp.media = {
                    'time_exec': _time,
                    'task_id': None,
                    'task_status': None,
                    'result': None
                    }
                resp.status = falcon.HTTP_400

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')