import falcon
from celery.result import AsyncResult
import time

from tasks import run
from utils import get_logger, LOG_LEVEL
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
        self.model_uri = None
        self.model_point = None,

    def on_post(self, req, resp):

        self.response = {},

        required_fields = {'dataset', 'metadata', 'model_config','model_type','period', 'task_id', 'model_point', 'model_uri'}
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
                    self.response = task.result
                    logger.debug(f'Got response from celery task: {self.response}')
                    self.task_state = task.status
                except Exception as err:
                    logger.error(f'Broker call has error: {err}')
                    resp.status = falcon.HTTP_500
            
            elif self.task_id is None:
                try:
                    task = run.delay(request)
                    self.task_state = "DEPLOYED"
                    self.task_id = task.id
                except Exception as err:
                    self.task_state = "FAILED"
                    logger.error(f'Task call with error: {err}')
                    resp.status = falcon.HTTP_500
        else:
            self.task_state = "FIELDS MISMATCH"
            logger.info(self.task_state)

        self.response['task_created'] = self.ts
        self.response['task_state'] = self.task_state
        self.response['task_id'] = self.task_id

        logger.debug(self.response)
        resp.media = self.response

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())