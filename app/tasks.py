from dotenv import load_dotenv
from pathlib import Path
import yaml
import celery
import os

from service import ModelAPI, ModelEnv

import configparser

from helper import get_logger, LOG_LEVEL, PATH_TO_CONFG
logger = get_logger(__name__, loglevel=LOG_LEVEL)

config = configparser.ConfigParser()
config.read_file(open(PATH_TO_CONFG))

CELERY_BROKER = config.get('CELERY', 'CELERY_BROKER')
CELERY_BACKEND = config.get('CELERY', 'CELERY_BACKEND')
# PATH_TO_MODEL_ENV = config.get('APP', 'PATH_TO_MODEL_ENV')
CONFIG_FILEPATH = config.get('APP', 'SERVICES_CONF')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

@app.task
def predict(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point')

    file_path = CONFIG_FILEPATH
    
    with open(file_path, 'r') as fl:

        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[model_type]
        logger.debug(f'Service config: {service_config}')
        
        env = ModelEnv(service_config)
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
