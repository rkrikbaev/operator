import yaml
import celery
import os

from service import Service
from utils import get_logger, LOG_LEVEL, CELERY_BROKER, CELERY_BACKEND, MLSERV_CONFIG_FILE, MODELS_REG, APP_CODE

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

    logger.debug(config)

    service = Service(config)

    exp_id = request.get('experiment_id')
    run_id = request.get('run_id')

    model_uri = get_model_path(MODELS_REG, exp_id, run_id=run_id)
    app_code_path = f'{APP_CODE}/{model_type}/app'
    model_hub = MODELS_REG

    logger.debug(f'{model_uri}')
    logger.debug(f'{app_code_path}')

    response = service.run(request,
                           model_point,
                           model_hub,
                           model_uri, 
                           app_code_path=app_code_path)

    logger.debug(f'Servise response in tasks.run: {response}')

    return response


def get_model_path(modelhub, exp_id, run_id=None, timestamp = 0, suffix='mlmodel'):

    if run_id is None:

        os.chdir(f'{modelhub}/{exp_id}')

        all_folders = [ x for x in os.listdir('.') if os.path.isdir(x) ]

        for folder in all_folders:
            tokens = folder.split('@')  
            
            if len(tokens) == 2:
                if int(timestamp) <= int(tokens[0]):
                    timestamp = tokens[0]
                    run_id = folder
    
        if run_id is None: 
            raise RuntimeError('Din not find any directory')
        else:
            print('Variable "run_id" is None so take latest saved model')

    path = f'{modelhub}/{exp_id}/{run_id}/{suffix}'
    
    if os.path.isdir(path): 
        return path
    else:
        raise RuntimeError(f'Could not find model by {path}')