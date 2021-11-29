import falcon
from falcon.media.validators import jsonschema

from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError

import time, json, os
import requests

from config.logger import logger
from schemas import load_schema
from config.storage import storage
from logic import Operator

HOST_NAME = os.getenv('HOST_NAME', default='operator')

TENSORFLOW_IMAGE = os.getenv('TENSORFLOW_IMAGE', default='tensorflow/serving:latest')
PROPHET_IMAGE = os.getenv('PROPHET_IMAGE', default='fpcloud/prophet-service:latest')

PATH_TO_MODELS = os.getenv('PATH_TO_MODELS', default='/tmp/models/')
PATH_TO_SOURCE = os.getenv('PATH_TO_SOURCE', default='/tmp/source')


# Make prediction
class Prediction():

    def __init__(self):
        
        self.config = None
        
    @jsonschema.validate(req_schema=load_schema('request'))
    def on_post(self, req, resp):

        resp.status = falcon.HTTP_400
        data =  req.media
    
        try:

            resp.media = self.mainFlow(data)
            resp.status = falcon.HTTP_201
        
        except Exception as error:
            logger.error(error)
            resp.status = falcon.HTTP_500

    def mainFlow(self, data):
        
        self.mtype = data.get('metadata').get('type')

        result = None
        self.container = None

        if self.connect():

            self.selectPlatform(self.mtype)

            result = self.container.run_steps(
                data,
                config = self.config,
                service = self.service
                )
        
        return result

    def selectPlatform(self, mtype):
        
        if mtype == 'prophet':

            self.service = ProphetService()
            self.config = self.service.config

        elif mtype == 'tensorflow':

           self.service = None
        
        else:
            self.config = None
            logger.warning('model type not supported')
            raise RuntimeError

    def connect(self):

        try:

            self.container = Operator()

            return True
        
        except (APIError, DockerException) as exc:
            
            logger.error(f'Cannot connect to Docker API due to {0}', exc)

            return False  


class ProphetService():

    APP_PORT = '8005/tcp'
    PROPHET_IMAGE = os.getenv('PROPHET_IMAGE', default='fpcloud/prophet-service:latest') 
    # PATH_TO_DEST = '/application'

    config = {
            'host_name': HOST_NAME,
            'app_port': APP_PORT,
            'image': PROPHET_IMAGE,
            # 'volumes': {PATH_TO_SOURCE: {"bind": PATH_TO_DEST, "mode": "rw"}},
            'detach': True
        }

    def __init__(self, **kwargs):
        pass     
    
    
    def handleRequest(self, data):
        
        future_data = data.get('future')
        history_data = data.get('history')
        settings = data['metadata'].get('settings')

        history_data_columns = ['ds','y']
        future_data_columns = ['ds']

        if (len(history_data[0]) - 1) != len(future_data[0]):

            logger.error('History data columns must include regressor data')
            raise RuntimeError 

        len_of_future_data = len(future_data[0])

        for index in range(0, len_of_future_data):
            
            if index > 0:

                regressor_id = f'x{index}'
                history_data_columns.append(regressor_id)
                future_data_columns.append(regressor_id)

        history = self.process_data(
            sample=history_data,
            columns=history_data_columns
        )
        
        future = self.process_data(
            sample=future_data,
            columns=future_data_columns
        )

        payload = {
                'settings': settings,
                'history': history,
                'future': future
            }

        return payload

    def call(self, host, port, payload=None):

        count = 0

        while True:

            try:

                headers = {"Content-Type": "application/json"}
                body = json.dumps(payload)
                url = f'http://{host}:{port}/action'
                
                response = requests.request("POST", url, headers=headers, data=body)
                
                return eval(response.text)

            except Exception as exp:
                
                count = count + 1

                logger.error(exp)
                time.sleep(2)

                if count > 5:

                    raise exp

    def process_data(self, sample, columns=None):

        result = {}

        if len(sample) == 0:
            raise RuntimeError
        
        if len(sample[0]) != len(columns):
            raise RuntimeError
        
        else:
            result = {'data': sample, 'columns': columns}

        return result

