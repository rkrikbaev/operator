import celery
import os
import time

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

docker_engine = DockerOperator()
service = ModelAsHTTPService()


@app.task
def predict(service_config, payload, point, model_id, model_features, regressor_names):

    port = service_config.get('port')
    logger.debug('try to create container with model')

    container_id, state = docker_engine.deploy_container(
        point, 
        service_config,
        model_features,
        model_id,
        regressor_names
        )

    logger.debug(f'Container state {container_id}')
    logger.debug(f'Container state {state}')

    if container_id and (state == 'running'):
        logger.debug(f'Make prediction with: {payload}, {point}, {port}')
        return service.call(payload, container_id, port, point)

    else:
        raise RuntimeError('Container not started...')
