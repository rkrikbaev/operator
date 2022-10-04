from dotenv import load_dotenv
from pathlib import Path

import os
import falcon
import mlflow

try:
    import model as prophet_model
except:
    pass


from helper import get_logger

logger = get_logger(__name__, loglevel='DEBUG')

dotenv_path = Path('./.env')
load_dotenv(dotenv_path=dotenv_path)

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
        logger.debug(request)
        
        experiment = request.get('model_point')

        #
        mlflow.set_experiment(experiment)
        experiment = mlflow.get_experiment_by_name(experiment)

        with mlflow.start_run(experiment_id=experiment.experiment_id):
        
            resp.media = self.model_instance.run(request)

api = falcon.App()

api.add_route("/health", CheckHealth())
api.add_route("/action", Predict())

logger.info("Server Loaded")