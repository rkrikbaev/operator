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
        resp.state = falcon.HTTP_200
        resp.media = {'status': 'ok'}

class Predict():
    def __init__(self):
        self.task_status = None, 
        self.task_id = None,
        self.model_uri = None
        self.model_point = None,

    def on_post(self, req, resp):

        self.response = { 
            "task_created": int(time.time()), 
            "model_uri": None, 
            "anomalies": None, 
            "prediction": None,
            "start_time": None,
            "finish_time": None,
            "model_point": None,
            "service_status": None,
            "model_status": None
            }

        resp.state = falcon.HTTP_200

        required_fields = {
            'dataset',
            'metadata',
            'model_config',
            'model_type',
            'period',
            'task_id',
            'model_point', 
            'model_uri'
            }
        request = req.media
        keys = set(request.keys())

        if required_fields == keys:

            self.task_id = request.get('task_id')
            self.model_point = request.get('model_point')
            # self.model_uri = request.get('model_uri')

            if self.task_id and len(self.task_id)>10:
                logger.debug(f'Request result from celery: {self.task_id}')

                try:
                    task = AsyncResult(self.task_id)
                    logger.debug(f'Result from celery task {self.task_id} : {task.result}')
                    logger.debug(f'status of the task: {self.task_id} : {task.status}')
                    self.response.update(task.result)
                    self.task_status = task.status
                except Exception as err:
                    logger.error(f'Broker call has error: {err}')
                    resp.status = falcon.HTTP_500
            
            elif self.task_id is None:
                try:
                    task = run.delay(request)
                    self.task_status = "DEPLOYED"
                    self.task_id = task.id
                except Exception as err:
                    self.task_status = "FAILED"
                    logger.error(f'Task call with error: {err}')
                    resp.status = falcon.HTTP_500
        else:
            self.response["service_status"] = "error at operator.api"
            logger.info(self.task_status)
            resp.status = falcon.HTTP_400
        
        self.response['task_status'] = self.task_status
        self.response['task_id'] = self.task_id
        self.response['model_point'] = self.model_point

        logger.debug(self.response)
        resp.media = self.response

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())