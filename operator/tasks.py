import yaml
import celery
import os

from service import Service
from utils import LOG_LEVEL, CELERY_BROKER, CELERY_BACKEND, MLSERV_CONFIG_FILE
import utils
logger = utils.get_logger(__name__, loglevel=LOG_LEVEL)

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

# app.conf.task_track_started = True

logger.debug(f'Create Celery object: {type(app)}')

@app.task(bind=True, track_started=True)
def run(self, request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point').lower()

    with open(MLSERV_CONFIG_FILE, 'r') as fl:
        config =  yaml.safe_load(fl).get('docker')[model_type] 

    logger.debug(f'Run Celery task for point: {model_point} with config: {config}')

    service = Service(config)

    self.update_state(state='DEPLOYED')

    response = service.run(request, model_point)

    return response