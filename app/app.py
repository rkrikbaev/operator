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
        point = request.get('model_point')
        _time = int(time.time())

        response_body = {
                    'time_exec': _time,
                    'task_status': None, 
                    'task_id': task_id,
                    'point': point,
                    'result':None
                    }

        resp.status = falcon.HTTP_200
        
        if task_id and len(task_id)>10:
            logger.debug(f'Request result from celery: {task_id}')

            try:
                task = AsyncResult(task_id)
                result = task.result
                status = task.status

                response_body['task_status'] = status
                response_body['task_id'] = task_id
                response_body['point'] = point
                response_body['result'] = result

            except Exception as err:
                logger.debug(f'Broker call has exception: {err}')
                resp.status = falcon.HTTP_500
    
        elif task_id is None:

            try:
                task = predict.delay(request)

                response_body['task_status'] = "DEPLOYED"
                response_body['task_id'] = task.id

            except Exception as err:
                logger.debug(err)
                resp.status = falcon.HTTP_500
        
        else:
                response_body['task_status'] = "FAILED"
                resp.status = falcon.HTTP_400

        resp.media = response_body

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')