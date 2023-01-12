import yaml
import celery
from service import Service
import configparser

from utils import get_logger, LOG_LEVEL, PATH_TO_CONFG
logger = get_logger(__name__, loglevel=LOG_LEVEL)

config = configparser.ConfigParser()
config.read_file(open(PATH_TO_CONFG))

CELERY_BROKER = config.get('CELERY', 'CELERY_BROKER')
CELERY_BACKEND = config.get('CELERY', 'CELERY_BACKEND')
CONFIG_FILEPATH = config.get('APP', 'SERVICES_CONF')

app = celery.Celery('tasks', broker=CELERY_BROKER, backend=CELERY_BACKEND)

@app.task
def run(request):

    model_type = request.get('model_type').lower()
    model_point = request.get('model_point')

    file_path = CONFIG_FILEPATH
    
    with open(file_path, 'r') as fl:

        f =  yaml.safe_load(fl)
        service_config = f.get('docker')[model_type]
        logger.debug(f'Service config: {service_config}')
        
        srv = Service(service_config)
        logger.debug(f'Create object: {type(srv)}')
        ip_address, state = srv.deploy(name= model_point.lower())

        logger.debug(f'Container: {model_point} has state {state}')
        
        if ip_address and (state == 'running'): service_response = srv.call(request)

        return service_response
