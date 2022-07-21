# project/app/tasks.py
import celery
import os
from time import sleep

import logging, sys
logging.basicConfig(stream=sys.stdout, level=logging.DEBUG,
                    format=f"%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s")
logger = logging.getLogger(__name__)

from service import ModelService

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

@app.task
def predict(service_config=None, data=None, task_id=None):

    mtype = data.get('type').lower()
    point = data.get('point').lower()

    payload = {
        'history': data.get('data'),
        'future': data.get('regressor'),
        'features': ["yhat", "yhat_lower", "yhat_upper"],
        'settings': data.get('config')
    }

    if service_config:

        service = ModelService(mtype, config=service_config)
        logger.debug(f'object created: {mtype}, {point}')

        try:
            feedback = service.call(payload, point)
            logger.debug(f'feedback on request: {feedback}')
        
        except Exception as error:
            logger.error(error)
    
    else:
        logger.warn('Service-config empty!') 
    