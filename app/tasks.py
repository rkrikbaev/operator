from dotenv import load_dotenv
from pathlib import Path
import yaml
import celery
import os

from service import ProphetModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
PATH_TO_MLRUNS = os.environ.get('PATH_TO_MLRUNS')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

s = ProphetModelAsHTTPService()

@app.task
def predict(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point').lower()
    model_features = request.get('model_features')
    regressor_names = request.get('regressor_names')
    model_uri = request.get('model_uri')
    model_config = request.get('model_config')
    dataset = request.get('dataset')
    period = request.get('period')

    logger.debug(f'Create new model: {model_uri}')
    logger.debug('Try to create container with model')

    path = Path(__file__).parent.absolute()
    file_path = os.path.join(path, 'service_config.yaml')
    service_config = None
    
    with open(file_path, 'r') as fl:
        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[model_type]

    docker = DockerOperator(service_config, path_to_models=PATH_TO_MLRUNS)
    ip_address, state = docker.deploy_container(model_point)

    logger.debug(f'Container: {model_point} has state {state}')
    if ip_address and (state == 'running'):

        payload = {
                    "model_point": model_point, 
                    "model_type": model_type,
                    "model_config": model_config,
                    "model_features": model_features,
                    "regressor_names": regressor_names,
                    "history_dataset": dataset, 
                    "period": period,
                    "model_uri": model_uri
                }

        logger.debug(f'Make prediction with: {payload}, {model_point}')
        
        response = s.call(payload, model_point, ip_address)

        return response
