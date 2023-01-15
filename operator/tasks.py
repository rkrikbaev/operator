import yaml
import celery
from service import Service

from utils import get_logger, LOG_LEVEL, CELERY_BROKER, CELERY_BACKEND, MLSERV_CONFIG_FILE

logger = get_logger(__name__, loglevel=LOG_LEVEL)

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)
logger.debug(f'Create Celery object: {type(app)}')

@app.task
def run(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point').lower()

    logger.debug(f'Deploy container with model for: {model_point}')
    with open(MLSERV_CONFIG_FILE, 'r') as fl:
        config =  yaml.safe_load(fl).get('docker')[model_type] 

    srv = Service(config)

    result = srv.run(model_point, request)
    logger.debug(f'Got result from model\'s service {result}')
    
    return result
