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
            "model_uri": None, 
            "anomalies": None, 
            "result": None,
            "start_time": None,
            "finish_time": None,
            "model_point": None,
            "service_status": None,
            "model_status": None,
            'task_status': None
            }

        resp.state = falcon.HTTP_200

        required_fields = {
            'dataset',
            'metadata',
            'model_config',
            'model_type',
            'period',
            'task_id',
            'task_status',
            'model_point',
            'model_tag',
            'model_run_id'
            }

        request = req.media
        keys = set(request.keys())

        if required_fields == keys:

            try:

                self.task_id = request.get('task_id')
                self.task_status = request.get('task_status')
                self.model_tag = request.get('model_tag')
                self.model_run_id = request.get('model_run_id')

                logger.debug(f'request: {request}')

                assert self.model_tag is not None

                logger.debug(self.task_id)
                logger.debug(self.task_status)

                if self.task_status == "PENDING":

                    logger.debug(f'get result from celery: {self.task_id}')

                    try:
                        task = AsyncResult(self.task_id)
                        
                        if isinstance(task.result, dict):

                            result = task.result
                            
                            logger.debug(result)
                            r = result.get('result')
                            if isinstance(r, list):
                                self.task_status = task.status
                        
                        self.response.update(result)
                    
                    except Exception as err:
                        logger.error(f'Broker call has error: {err}')
                        resp.status = falcon.HTTP_500

                elif self.task_status == "QUEUED":

                    try:
                        task = run.delay(request)
                        self.task_id = task.id
                        self.task_status = task.status

                    except Exception as err:
                        logger.error(f'Task call with error: {err}')
                        resp.status = falcon.HTTP_500

                else:
                    resp.status = falcon.HTTP_400

                self.response['task_status'] = self.task_status
                self.response['task_id'] = self.task_id
                self.response['model_tag'] = self.model_tag

                logger.debug(f'response: {self.response}')
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
