import celery
import os
import time

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
TRACKING_SERVER = os.environ.get('TRACKING_SERVER')
tracking_server = TRACKING_SERVER
print(tracking_server)

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

docker_engine = DockerOperator()
service = ModelAsHTTPService()


@app.task
def predict(service_config, payload, point, model_id, model_features, regressor_names):
    
    global tracking_server

    if tracking_server is None:
        tracking_server = 'http://mlflow:5000'

    port = service_config.get('port')
    logger.debug('try to create container with model')
    
    try:
        container_info = docker_engine.deploy_container(
            point, 
            service_config,
            model_features,
            tracking_server,
            model_id,
            regressor_names
            )

        time.sleep(5)

        container_id = container_info.get('id')
        state = container_info.get('state')

        logger.debug(f'Container state {container_info}')

        if container_id and (state == 'running'):

            logger.debug(f'Make prediction with: {payload}, {point}, {port}')
            result = service.call(payload, point, port)
            logger.debug(f'Feedback on request: {result}')
            return result
        
        else:
            raise RuntimeError('Container not started...')
    
    except RuntimeError as error:
        logger.error(error)
    
    finally:
        pass
        # docker_engine.remove_container(point)