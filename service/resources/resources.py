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
        print('on_post')
        resp.status = falcon.HTTP_400
        data =  req.media
        logger.debug(data)
        try:
            resp.media = self.mainFlow(data)
            resp.status = falcon.HTTP_201

        except Exception as error:
            logger.error(error)
            resp.status = falcon.HTTP_500

    def mainFlow(self, data):

        logger.debug('start main flow')
        self.mtype = data.get('model_settings').get('type')

        self.container = None

        if self.connect():

            self.selectPlatform(self.mtype)

            return  self.container.run_steps(
                data,
                config = self.config,
                service = self.service
                )

    def selectPlatform(self, mtype):

        if mtype.lower() == 'prophet':

            self.service = ProphetService()
            self.config = self.service.config

        elif mtype.lower() == 'tensorflow':

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
    PROPHET_IMAGE = os.getenv('PROPHET_IMAGE', default='fpcloud/prophet-service-amd64:latest') 
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

        logger.debug(f'keys in data: {list(data.keys())}')

        future_data = data.get('regressor')
        history_data = data.get('data')
        settings = data.get('model_settings').get('config')

        if (isinstance(settings, str)):
           settings = json.loads(settings)
        elif (isinstance(settings, dict)):
           pass
        else:
           raise ValueError

        logger.debug(f'keys in settings: {list(settings.keys())}')

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

    def call(self, container_name, app_port=None, payload=None):

        if not(isinstance(app_port, int)):
           logger.error('appliaction port must be integer')
           raise TypeError

        count = 0
        logger.info(f'call running model: {container_name}')
        while True:

            try:

                headers = {"Content-Type": "application/json"}
                body = json.dumps(payload)
                url = f'http://{container_name}:{app_port}/action'
                response = requests.request("POST", url, headers=headers, data=body)
                logger.debug(f'response from the model: {container_name}')
                return eval(response.text)

            except Exception as exp:

                count = count + 1

                logger.error(exp)
                time.sleep(2)

                if count > 5:

                    raise exp

    def process_data(self, sample, columns=None):

        if len(sample) == 0:
            raise RuntimeError

        if len(sample[0]) != len(columns):
            raise RuntimeError

        else:
            return {'data': sample, 'columns': columns}
