from dotenv import load_dotenv

import celery
import os

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

load_dotenv()

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
TRACKING_SERVER = os.environ.get('TRACKING_SERVER')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

docker_engine = DockerOperator()
service = ModelAsHTTPService()
tracking_server = TRACKING_SERVER

@app.task
def predict(service_config, request, point):

    model_features = request.get('features')
    regressor_names = request.get('regressor_names')
    model_uri = request.get('model_uri')
    
    payload = {
        'data': request.get('request')
    }

    port = service_config.get('port')

    try:

        container_info = docker_engine.deploy_container(
            point, 
            service_config,
            model_features,
            tracking_server,
            model_uri,
            regressor_names
            )

        container_id = container_info.get('id')
        
        if container_id:
            result = service.call(payload, point, port)
            logger.debug(f'feedback on request: {result}')
            return result
        else:
            raise RuntimeError('Container not started...')
    
    except RuntimeError as error:
        logger.error(error)
    
    finally:
        docker_engine.remove_container(point)

    