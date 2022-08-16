from dotenv import load_dotenv
from pathlib import Path

import celery
import os

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

service = ModelAsHTTPService()

@app.task
def predict(service_config, payload, point, model_id, model_features, regressor_names):

    logger.debug('Try to create container with model')
    docker_engine = DockerOperator(service_config)

    ip_address, state = docker_engine.deploy_container(
        point,
        model_features,
        model_id,
        regressor_names
        )

    logger.debug(f'Container: {point} has state {state}')

    if ip_address and (state == 'running'):
        logger.debug(f'Make prediction with: {payload}, {point}')
        
        return service.call(payload, point, ip_address)
