import yaml
import celery
import os

from service import Service
from utils import LOG_LEVEL, CELERY_BROKER, CELERY_BACKEND, MLSERV_CONFIG_FILE, MODELS_REG, APP_CODE
import utils
logger = utils.get_logger(__name__, loglevel=LOG_LEVEL)

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

    exp_id = request['model_uri'].get('experiment_id')
    run_id = request['model_uri'].get('run_id')

    model_uri = utils.find_model(local_path=MODELS_REG, 
                                 remote_path='/mlruns', 
                                 exp_id=exp_id, 
                                 run_id=run_id)

    request['model_uri'], response['model_uri'] = model_uri

    app_code_path = f'{APP_CODE}/{model_type}/app'

    logger.debug(f'{model_uri}')
    logger.debug(f'{app_code_path}')

    response = service.run(request,
                           model_point,
                           MODELS_REG,
                           app_code_path=app_code_path)

    logger.debug(f'Servise response in tasks.run: {response}')

    return response