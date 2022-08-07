import celery
import os

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
    
    tracking_server = os.environ.get('TRACKING_SERVER')
    port = service_config.get('port')
    
    try:
        container_info = docker_engine.deploy_container(
            point, 
            service_config,
            model_features,
            tracking_server,
            model_id,
            regressor_names
            )
        
        container_id = container_info.get('id')
        logger.debug(f'Container created {container_id}')

        if container_id:
            logger.debug(f'Make prediction')
            result = service.call(payload, point, port)
            logger.debug(f'feedback on request: {result}')
            return result
        else:
            raise RuntimeError('Container not started...')
    
    except RuntimeError as error:
        logger.error(error)
    
    finally:
        pass
        # docker_engine.remove_container(point)