from dotenv import load_dotenv
from pathlib import Path
import yaml
import celery
import os

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
PATH_TO_MLRUNS = os.environ.get('PATH_TO_MLRUNS')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

s = ModelAsHTTPService()

@app.task
def predict(request):
    mtype = request.get('type').lower()
    point = request.get('point').lower()
    model_features = request.get('features')
    regressor_names = request.get('regressor_names')
    model_id = request.get('model_id')
    payload = request.get('data')

    logger.debug(f'Create new model: {model_id}')
    logger.debug('Try to create container with model')

    path = Path(__file__).parent.absolute()
    file_path = os.path.join(path, 'service_config.yaml')
    service_config = None
    
    with open(file_path, 'r') as fl:
        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[mtype]

    docker = DockerOperator(service_config, path_to_models=PATH_TO_MLRUNS)

    ip_address, state = docker.deploy_container(
        point,
        model_features,
        model_id,
        regressor_names
        )

    logger.debug(f'Container: {point} has state {state}')

    if ip_address and (state == 'running'):
        logger.debug(f'Make prediction with: {payload}, {point}')
        
        return s.call(payload, point, ip_address)
