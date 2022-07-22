# project/app/tasks.py
import celery
import os
from time import sleep

from middleware.helper import logger
logger = logger(__name__)

from service import ModelService, DockerOperator

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

@app.task
def predict(config, data=None, task_id=None):

    mtype = data.get('type').lower()
    point = data.get('point').lower()

    payload = {
        'history': data.get('data'),
        'future': data.get('regressor'),
        'features': ["yhat", "yhat_lower", "yhat_upper"],
        'settings': data.get('config')
    }

    docker_config = config.get('docker')
    port = config.get('port')
    # mtype = mtype

    if config:
        
        container = DockerOperator()
        container_id = container.deploy(mtype=mtype, port=port, point=point, config=docker_config[mtype])

        service = ModelService(mtype, config=config)

        logger.debug(f'object created: {mtype}, {point}')

        try:
            feedback = service.call(payload, point)
            logger.debug(f'feedback on request: {feedback}')
        
        except Exception as error:
            logger.error(error)
        finally:
            container.remove(container_id)
    else:
        logger.warn('Service-config empty!') 
    