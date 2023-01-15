import falcon
# import mlflow
from pathlib import Path
from model import Model
from utils import get_logger, LOG_LEVEL, TRACKING_SERVER

logger = get_logger(__name__, loglevel=LOG_LEVEL)


class Health():
    def on_get(self, req, resp):
        resp.media = "ok"

class Action:
    def __init__(self) -> None:
        self.model = Model(tracking_server=TRACKING_SERVER)

    def on_post(self, req, resp):
        request = req.media
        logger.debug(f'Request from the operator: {request}')
        resp.state = falcon.HTTP_400
        response = {
            "state": resp.state,
            "prediction": None,
            "model_uri": None,
            "anomalies": None,
            "model_uri": None
            }

        required_fields = {'model_config', 'dataset', 'model_uri', 'metadata', 'period', }
        keys = set(request.keys())

        if required_fields == keys:
            resp.state = falcon.HTTP_500
            response['state'] = resp.state 
            # experiment = request.get('model_point')
            config = request.get('model_config')
            metadata = request.get('metadata')
            data = request.get('dataset')
            model_uri = request.get('model_uri')

            experiment_id = model_uri.get('experiment_id')
            run_id = model_uri.get('run_id')

            model_uri = f'/opt/mlruns/{experiment_id}/{run_id}/mlmodel'

            # add experiment as the point
            # mlflow.set_experiment(experiment)
            # experiment = mlflow.get_experiment_by_name(experiment)

            # with mlflow.start_run(experiment_id=experiment.experiment_id):
            
            result = self.model.run(data, config, model_uri),

            response["prediction"]: result
            response["model_uri"]: model_uri
            response["anomalies"]: None
            
            logger.debug(f'Model response: {response}')
            resp.state = falcon.HTTP_200
        
        resp.media = response

api = falcon.App()

api.add_route("/health", Health())
api.add_route("/action", Action())

logger.info("Service started")