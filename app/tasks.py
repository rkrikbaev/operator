from dotenv import load_dotenv
from pathlib import Path
import yaml
import celery
import os

from service import ModelAsHTTPService, DockerController

from helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
PATH_TO_MLRUNS = os.environ.get('PATH_TO_MLRUNS')
PATH_TO_MODEL_ENV = os.environ.get('PATH_TO_MODEL_ENV', default='/usr/local/etc/operator')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

model_service = ModelAsHTTPService()

@app.task
def predict(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point').lower()
    metadata = request.get('metadata')
    model_features = metadata.get('model_features')
    regressor_names = metadata.get('regressor_names')
    model_uri = request.get('model_uri')
    model_config = request.get('model_config')
    dataset = request.get('dataset')
    period = request.get('period')


    path = Path(__file__).parent.absolute()
    file_path = os.path.join(path, 'service_config.yaml')
    
    with open(file_path, 'r') as fl:
        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[model_type]

        docker = DockerController(service_config, model_type, path_to=PATH_TO_MODEL_ENV)
        ip_address, state = docker.deploy_container(model_point)

        logger.debug(f'Container: {model_point} has state {state}')
        if ip_address and (state == 'running'):

            payload = {
                        "model_point": model_point, 
                        "model_type": model_type,
                        "model_config": model_config,
                        "model_features": model_features.split(','),
                        "regressor_names": regressor_names.split(','),
                        "dataset": dataset, 
                        "period": period,
                        "model_uri": model_uri
                    }

            logger.debug(f'Make prediction with: {payload}, {model_point}')
            
            response = model_service.call(payload, model_point, ip_address)

            return response
