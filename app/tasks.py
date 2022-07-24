import celery
import os

from service import ModelAsHTTPService, DockerOperator

from middleware.helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)
# logger.debug(f'connected to CELERY_BROKER {CELERY_BROKER} and CELERY_BACKEND {CELERY_BACKEND}')

@app.task
def predict(config, data):

    mtype = data.get('type').lower()
    point = data.get('point').lower()

    payload = {
        'history': data.get('data'),
        'future': data.get('regressor'),
        'features': ["yhat", "yhat_lower", "yhat_upper"],
        'settings': data.get('config')
    }

    service_config = config.get('docker')
    port = config.get('port')

    # do something if config empty, better use ParseConfig!
    if config:
        pass
    else:
        logger.warn('Service-config empty!')  
              
    container = DockerOperator()
    service = ModelAsHTTPService()
    container_id = None
    config = service_config[mtype]
    logger.debug(f'object created: {mtype}, {point}')

    try:
        container_info = container.deploy(port=port, point=point, config=config)
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
        container.remove(point)

    