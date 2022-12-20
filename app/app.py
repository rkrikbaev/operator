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

        self.response_body = {
            'ts': None,
            'task_status': None, 
            'task_id': None,
            'point': None,
            'result':None
            }

    def on_post(self, req, resp):

        required_fields = {'dataset', 'metadata', 'model_config','model_type','model_uri','period', 'task_id', 'model_point'}
        self.response_body['ts'] = int(time.time())
        resp.status = falcon.HTTP_200

        request = req.media
        keys = set(request.keys())

        if required_fields == keys:

            task_id = request.get('task_id')
            point = request.get('model_point')

            self.response_body['task_id'] = task_id
            self.response_body['model_point'] = point

            if task_id and len(task_id)>10:
                logger.debug(f'Request result from celery: {task_id}')

                try:
                    task = AsyncResult(task_id)
                    self.response_body['task_status'] = task.status
                    self.response_body['result'] = task.result

                except Exception as err:
                    logger.debug(f'Broker call has exception: {err}')
                    resp.status = falcon.HTTP_500
        
            elif task_id is None:
                try:
                    task = predict.delay(request)
                    self.response_body['task_status'] = "DEPLOYED"
                    self.response_body['task_id'] = task.id

                except Exception as err:
                    logger.debug(err)
                    resp.status = falcon.HTTP_500
        else:

            self.response_body['task_status'] = "FAILED"
            resp.status = falcon.HTTP_400

        resp.media = self.response_body

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')