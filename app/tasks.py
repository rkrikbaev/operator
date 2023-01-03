from dotenv import load_dotenv
from pathlib import Path
import yaml
import celery
import os

from service import ModelAPI, ModelEnv

import configparser
config = configparser.ConfigParser()
config.read_file(open(r'app/main.config'))

from helper import get_logger, LOG_LEVEL
logger = get_logger(__name__, loglevel=LOG_LEVEL)

CELERY_BROKER = config.get('CELERY', 'CELERY_BROKER')
CELERY_BACKEND = config.get('CELERY', 'CELERY_BACKEND')
# PATH_TO_MODEL_ENV = config.get('APP', 'PATH_TO_MODEL_ENV')
CONFIG_FILEPATH = config.get('APP', 'CONFIG_FILEPATH')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

@app.task
def predict(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point')

    # path = Path(__file__).parent.absolute()
    # file_path = os.path.join(path, 'service_config.yaml')
    file_path = CONFIG_FILEPATH
    
    with open(file_path, 'r') as fl:

        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[model_type]

        env = ModelEnv(**service_config)
        ip_address, state = env.deploy_container(model_point.lower())

        logger.debug(f'Container: {model_point} has state {state}')
        
        if ip_address and (state == 'running'):
            
            metadata = request.get('metadata')
            model_uri = request.get('model_uri')
            model_config = request.get('model_config')
            dataset = request.get('dataset')
            period = request.get('period')
            
            payload = {
                        "model_point": model_point, 
                        "model_type": model_type,
                        "model_config": model_config,
                        "metadata": metadata,
                        "dataset": dataset, 
                        "period": period,
                        "model_uri": model_uri
                    }

            # logger.debug(f'Make prediction with: {payload}, {model_point}')
            model_service = ModelAPI(model_point, ip_address)
            return model_service.call(payload)
