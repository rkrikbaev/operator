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
    
    tracking_server=os.environ.get('TRACKING_SERVER')
    logger.debug(tracking_server)

    port = service_config.get('port')
    logger.debug('try to create container with model')

    if tracking_server:
        pass
    else:
        raise RuntimeError('Address of tracking server was not set')

    container_id, state = docker_engine.deploy_container(
        point, 
        service_config,
        model_features,
        model_id,
        regressor_names,
        tracking_server=tracking_server
        )

    logger.debug(f'Container state {container_id}')
    logger.debug(f'Container state {state}')

    if container_id and (state == 'running'):
        logger.debug(f'Make prediction with: {payload}, {point}, {port}')
        return service.call(payload, point, port)

    else:
        raise RuntimeError('Container not started...')
