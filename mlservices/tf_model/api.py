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
        logger.debbug(f'Request from the operator: {request}')

        list_of_keys = ['model_config', 'dataset']
        if all(k in request for k in list_of_keys):

            # experiment = request.get('model_point')
            config = request.get('model_config')
            metadata = request.get('metadata')
            data = request.get('dataset')

            experiment_id = metadata.get('experiment_id')
            run_id = metadata.get('run_id')

            path_abs = Path(__file__).parent.absolute()
            model_uri = f'{path_abs}/mlruns/{experiment_id}/{run_id}/mlmodel'

            # add experiment as the point
            # mlflow.set_experiment(experiment)
            # experiment = mlflow.get_experiment_by_name(experiment)

            # with mlflow.start_run(experiment_id=experiment.experiment_id):
            resp.media = self.model.run(data, config=config, model_uri=model_uri)
        else:
            resp.state = falcon.HTTP_400
            logger.info(resp.state)

api = falcon.App()

api.add_route("/health", Health())
api.add_route("/action", Action())

logger.info("Service started")