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
    model_point = request.get('model_point')
    response = None
    
    with open(MLSERV_CONFIG_FILE, 'r') as fl:
        config =  yaml.safe_load(fl).get('docker')[model_type] 

    srv = Service(config)
    logger.debug(f'Create object Service: {type(srv)}')

    ip_address, state = srv.deploy(name= model_point.lower())
    logger.debug(f'Container: {model_point}, state: {state}')

    if ip_address and (state == 'running'): response = srv.call(request)
    return response
