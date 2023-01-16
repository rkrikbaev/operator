from dotenv import load_dotenv
from pathlib import Path

import os
import falcon
import mlflow

try:
    import model as prophet_model
except:
    raise RuntimeError('Cannot import service to run the model')

LOG_LEVEL = os.environ.get('LOG_LEVEL')
if LOG_LEVEL==None:
    LOG_LEVEL='INFO'

from helper import get_logger
logger = get_logger(__name__, loglevel=LOG_LEVEL)

# dotenv_path = Path('./.env')
# load_dotenv(dotenv_path=dotenv_path)

TRACKING_SERVER = os.getenv('TRACKING_SERVER', default='http://mlflow:5000')

# LIST_OF_SUPPORTED_MODELS = os.getenv('LIST_OF_SUPPORTED_MODELS', default={
#     'prophet_model': prophet_model.Model(tracking_server=TRACKING_SERVER),
#     'tensorflow_model': tensorflow_model.Model(tracking_server=TRACKING_SERVER)
#     })


class CheckHealth():

    def on_get(self, req, resp):
        resp.media = "ok"

class Predict:
    
    def __init__(self) -> None:
        self.model_instance = prophet_model.Model(tracking_server=TRACKING_SERVER)

    def on_post(self, req, resp):
        
        request = req.media
        
        list_of_keys = ['model_config', 'dataset', 'period', 'metadata', 'model_uri']
        if all(k in request for k in list_of_keys):

            experiment = request.get('model_point')

            # add experiment as point
            mlflow.set_experiment(experiment)
            experiment = mlflow.get_experiment_by_name(experiment)

            with mlflow.start_run(experiment_id=experiment.experiment_id):
                resp.media = self.model_instance.run(request)
        else:
            resp.state = falcon.HTTP_400
            logger.info(resp.state)

api = falcon.App()

api.add_route("/health", CheckHealth())
api.add_route("/action", Predict())

logger.info("Service started")