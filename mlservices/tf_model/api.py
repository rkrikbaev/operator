import os
import falcon
import mlflow

from model import Model
from utils import get_logger, LOG_LEVEL

logger = get_logger(__name__, loglevel=LOG_LEVEL)
TRACKING_SERVER = os.getenv('TRACKING_SERVER', default='http://mlflow:5000')

class Health():
    def on_get(self, req, resp):
        resp.media = "ok"

class Action:
    def __init__(self) -> None:
        self.model = Model(tracking_server=TRACKING_SERVER)

    def on_post(self, req, resp):
        request = req.media
        
        list_of_keys = ['model_config', 'dataset', 'period', 'metadata']
        if all(k in request for k in list_of_keys):

            experiment = request.get('model_point')
            config = request.get('model_config')
            metadata = request.get('metadata')
            data = request.get('dataset')

            window = config.get('window')
            experiment_id = metadata.get('experiment_id')
            run_id = metadata.get('run_id')

            # add experiment as the point
            mlflow.set_experiment(experiment)
            experiment = mlflow.get_experiment_by_name(experiment)

            with mlflow.start_run(experiment_id=experiment.experiment_id):
                resp.media = self.model.run(data, window=window, experiment_id=experiment_id, run_id=run_id)
        else:
            resp.state = falcon.HTTP_400
            logger.info(resp.state)

api = falcon.App()

api.add_route("/health", Health())
api.add_route("/action", Action())

logger.info("Service started")