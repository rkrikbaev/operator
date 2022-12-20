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
        self.ts = None,
        self.task_status = None, 
        self.task_id = None,
        self.model_point = None,
        self.result = None,
        self.model_uri = None

    def on_post(self, req, resp):

        required_fields = {'dataset', 'metadata', 'model_config','model_type','model_uri','period', 'task_id', 'model_point'}
        self.ts = int(time.time())
        resp.status = falcon.HTTP_200

        request = req.media
        keys = set(request.keys())

        if required_fields == keys:

            self.task_id = request.get('task_id')
            self.point = request.get('model_point')
            self.model_uri = request.get('model_uri')

            if self.task_id and len(self.task_id)>10:
                logger.debug(f'Request result from celery: {self.task_id}')

                try:
                    task = AsyncResult(self.task_id)
                    self.result = task.result
                    self.model_uri = task.result.get('model_uri')
                    self.task_status = task.status
                    
                except Exception as err:
                    logger.debug(f'Broker call has exception: {err}')
                    resp.status = falcon.HTTP_500
        
            elif self.task_id is None:
                try:
                    task = predict.delay(request)
                    self.task_status = "DEPLOYED"
                    self.task_id = task.id

                except Exception as err:
                    logger.debug(err)
                    resp.status = falcon.HTTP_500
        else:

            self.task_status = "FAILED"
            resp.status = falcon.HTTP_400
        
        _response = {
            'ts': self.ts,
            'task_status': self.task_status, 
            'task_id': self.task_id,
            'model_point': self.model_point,
            'result':self.result,
            'model_uri': self.model_uri
            }
        logger.debug(_response)
        resp.media = _response
        
api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')