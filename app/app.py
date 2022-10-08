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

        try:
            logger.debug('Health check')
            resp.status = falcon.HTTP_200
            resp.media = {'state': 'ok'}
        except:
            resp.status = falcon.HTTP_500


class Predict():

    def __init__(self):
        pass

    def on_post(self, req, resp):

        request = req.media
        task_id = request.get('task_id')
        _time = int(time.time())
        logger.debug(request)
        
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

                resp.status = falcon.HTTP_200

            except Exception as err:
                logger.debug(f'Broker call has exception: {err}')
                resp.status = falcon.HTTP_500
                resp.media = {'state': 'error', 'error_text': str(err)}
    
        else:
            try:

                task = predict.delay(request)

                resp.media = {
                    'time_exec': _time,
                    'task_id': task.id,
                    'task_status': "DEPLOYED",
                    'result': None
                    }

                logger.debug(f'"ts": {time.ctime()},"task_id": {task.id}, "state":"success"')
            
            except Exception as err:
                logger.debug(err)
                resp.status = falcon.HTTP_500
                resp.media = {'state':'error', 'error_text': str(err)}

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')