#!.venv/bin/python

import falcon
from falcon.media.validators import jsonschema
from celery.result import AsyncResult
import time

from schemas import schema
from tasks import predict

import logging, yaml, os
try:
    file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'config/logger_config.yaml')
    with open(file_path, 'r') as fl:
        config =  yaml.safe_load(fl)
        logging.config.dictConfig(config)      
except:
    pass
finally:
    logger = logging.getLogger(__name__)

class Health():

    def __init__(self):
        pass

    def on_get(self, req, resp):

        try:
            print('Health check')
            resp.status = falcon.HTTP_200
            resp.media = {'state': 'ok'}
        except:
            resp.status = falcon.HTTP_500

class Predict():

    def __init__(self):
        pass

    @jsonschema.validate(req_schema=schema.load_schema('request'))
    def on_post(self, req, resp, task_id):

        logger.debug(f'Request received for task with id: {task_id}')

        if task_id:

            task_result = AsyncResult(task_id)
            result = {'status': task_result.status, 'result': task_result.result}
            resp.status = falcon.HTTP_200
            response = result
        
        else:
            file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'config/service_config.yaml')
            logger.debug(f'Loaded config file : {file_path}')
            config = load_config(file_path)
            data =  req.media
            state = predict(config, data) 
            response = {'task_id': int(time.ctime()), 'state':state}
            
            # logger.debug(f'Incoming data: [history: {data.get("data")[0:2]}, future: {data.get("regressor")[0:2]}, settings: {data.get("config")}, features: {data.get("features")}]')
            # logger.debug(f'Incoming data: [history: {len(data.get("data"))}, future: {len(data.get("regressor"))}, settings: {data.get("config")}, features: {data.get("features")}]')      
        
        resp.media = response