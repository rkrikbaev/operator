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
def predict(config, data):

    mtype = data.get('type').lower()
    point = data.get('point').lower()
    tracking_server_uri = config.get('tracking_server_uri')

    payload = {
        'future': data.get('regressor'),
        'features': ["yhat", "yhat_lower", "yhat_upper"],
        "model_uri":"dc9639f797cc4c1baf280926757c72c5",
        "tracking_server_uri":tracking_server_uri,
        "regressor_names":data.get('regressor')
    }

    service_config = config.get('docker')
    port = config.get('port')

    # do something if config empty, better use ParseConfig!
    if config:
        pass
    else:
        logger.warn('Service-config empty!')  
              
    # container = DockerOperator()
    # service = ModelAsHTTPService()
    container_id = None
    config = service_config[mtype]
    logger.debug(f'object created: {mtype}, {point}')

    try:
        container_info = docker_engine.deploy_container(port=port, point=point, config=config)
        container_id = container_info.get('id')
        if container_id:
            result = service.call(payload, point, port=port)
            logger.debug(f'feedback on request: {result}')
            return result
        else:
            raise RuntimeError('Container not started...')
    except RuntimeError as error:
        logger.error(error)
    finally:
        docker_engine.remove_container(point)

    