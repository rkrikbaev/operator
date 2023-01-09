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

TRACKING_SERVER = os.getenv('TRACKING_SERVER', default='http://138.68.70.41:5000')

class CheckHealth():

    def on_get(self, req, resp):
        resp.media = "ok"

class Predict:
    
    def __init__(self) -> None:
        self.model_instance = prophet_model.Model(tracking_server=TRACKING_SERVER)

    def on_post(self, req, resp):
        
        request = req.media
        
        list_of_keys = ['model_config', 'dataset', 'period', 'metadata']
        if all(k in request for k in list_of_keys):

            experiment = request.get('model_point')
            config = request.get('model_config')
            metadata = request.get('metadata')
            data = request.get('dataset')

            window = config.get('window')
            run_id = metadata.get('run_id')

            # add experiment as the point
            mlflow.set_experiment(experiment)
            experiment = mlflow.get_experiment_by_name(experiment)

            with mlflow.start_run(experiment_id=experiment.experiment_id):
                resp.media = self.model_instance.run(data, window=window, run_id=run_id)
        else:
            resp.state = falcon.HTTP_400
            logger.info(resp.state)

api = falcon.App()

api.add_route("/health", CheckHealth())
api.add_route("/action", Predict())

logger.info("Service started")