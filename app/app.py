import falcon
from celery.result import AsyncResult
import time

from tasks import predict
from helper import get_logger, LOG_LEVEL
logger = get_logger(__name__, loglevel=LOG_LEVEL)

logger.info(f'LOG_LEVEL: {LOG_LEVEL}')
            
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
        self.task_state = None, 
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
            self.model_point = request.get('model_point')
            self.model_uri = request.get('model_uri')

            if self.task_id and len(self.task_id)>10:
                logger.debug(f'Request result from celery: {self.task_id}')

                try:
                    task = AsyncResult(self.task_id)
                    self.result = task.result
                    self.model_uri = task.result.get('model_uri')
                    self.task_state = task.status
                    
                except Exception as err:
                    logger.error(f'Broker call has error: {err}')
                    resp.status = falcon.HTTP_500
        
            elif self.task_id is None:
                try:
                    task = predict.delay(request)
                    self.task_state = "DEPLOYED"
                    self.task_id = task.id
                except Exception as err:
                    self.task_state = "FAILED"
                    logger.error(f'Task call with error: {err}')
                    resp.status = falcon.HTTP_500
        else:
            self.task_state = "FIELDS MISMATCH"
            logger.info(self.task_state)
        
        response = {
            'ts': self.ts,
            'task_state': self.task_state, 
            'task_id': self.task_id,
            'model_point': self.model_point,
            'result':self.result,
            'model_uri': self.model_uri
            }

        logger.debug(response)
        resp.media = response

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())