import falcon
from celery.result import AsyncResult
import time

from tasks import run
from utils import LOG_LEVEL, MODELS_REG
import utils

logger = utils.get_logger(__name__, loglevel=LOG_LEVEL)

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
        self.model_point = None,
        self.model_run_id = None

    def on_post(self, req, resp):

        self.response = { 
            "task_updated": int(time.time()), 
            "model_uri": {}, 
            "anomalies": None, 
            "result": None,
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
            'model_path',
            'model_run_id'
            }

        request = req.media
        keys = set(request.keys())

        if required_fields == keys:

            try:

                self.task_id = request.get('task_id')
                self.model_path = request.get('model_path')
                self.model_run_id = request.get('model_run_id')
                #self.model_point = request.get('model_point')
                #self.response['model_uri'] = {"experiment_id": self.model_point, "run_id":self.model_run_id}

                if not self.model_path: raise RuntimeError('Model PATH not set')
                logger.debug(self.task_id)
                if self.task_id:

                    logger.debug(f'Request result from celery: {self.task_id}')

                    try:
                        task = AsyncResult(self.task_id)
                        if isinstance(task.result, dict):
                            self.response.update(task.result)
                        self.task_status = task.status
                    except Exception as err:
                        logger.error(f'Broker call has error: {err}')
                        resp.status = falcon.HTTP_500

                elif self.task_id is None:

                    try:
                        task = run.delay(request)
                        self.task_id = task.id
                        self.task_status = task.status
                    except Exception as err:
                        logger.error(f'Task call with error: {err}')
                        resp.status = falcon.HTTP_500

                self.response['task_status'] = self.task_status
                self.response['task_id'] = self.task_id
                self.response['model_path'] = self.model_path

                logger.debug(self.response)
                resp.media = self.response

            except RuntimeError as exc:
                logger.error(exc)
                resp.status = falcon.HTTP_500
        else:
            self.response["service_status"] = "bad request, required fields are missing"
            logger.info(self.response)
            resp.status = falcon.HTTP_400

api = falcon.App()

api.add_route('/predict', Predict())
api.add_route("/health", Health())
