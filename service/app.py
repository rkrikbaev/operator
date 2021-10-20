from config.logger import logger

import pandas as pd   # must be replaced with internal python tools!
import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound

import os
import time
import json
import requests
import simplejson

class Service():

    def __init__(self, config):
        
        #
        self.app_host = config.get('app_host')
        self.app_internal_port = config.get('app_internal_port')
        
        
        # docker container config
        self.container_config = None
        self.prophet_conatiner_config = config.get('prophet')
        self.tensorflow_conatiner_config = config.get('tensorflow')


        # self.prophet_object = CallProphetObject(self.app_host)

        # docker host url
        # self.base_url = base_url
        # self.timeout = timeout
        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
        except DockerException as exc:
            logger.error(exc)
            raise exc

        self.history = None
        self.future = None

        self.start = None
        self.stop = None
        self.container_id = None
        self.service_response = None
        self.container_state = None

    def _runContainer(self, container_config):
        
        '''Run a docker container using a given image; passing keyword arguments
        documented to be accepted by docker's client.containers.run function
        No extra side effects. Handles and reraises ContainerError, ImageNotFound,
        and APIError exceptions.
        '''
        image = container_config.get('image')
        volumes = container_config.get('volumes')
        con_int_ports = container_config.get('app_internal_port')   
        ports = {con_int_ports:None}
        container = None

        try:
            
            container = self.client.containers.run(image, ports=ports, volumes=volumes, detach=True)

            if "name" in container_config.keys():
                logger.info("Container", container_config["name"], "is now running.")

        except ContainerError as exc:
            logger.error("Failed to run container")
            raise exc
        except ImageNotFound as exc:
            logger.error("Failed to find image to run as a docker container")
            raise exc
        except APIError as exc:
            logger.error("Unhandled error")
            raise exc

        return container   

    def _removeContainer(self, container_id, **kwargs):
            
            '''
            Remove a docker container using a given id; passing keyword arguments
            documented to be accepted by docker's client.containers.remove function
            No extra side effects. Handles and reraises APIError exceptions.
            '''
                
            try:               
            
                container = self.client.containers.get(container_id=container_id)
                container.remove(force=True)
                self.container_id = None
                logger.info(f'Container {container_id} was removed')

            except APIError as exc:
                logger.error(f'Unhandled APIError error: {exc}')
                raise exc

    def _steps(self, callback):

            self.start = str(datetime.datetime.now())
            self.container_id = None

            count = 0
            container_running = None
            con_ext_port = None

            try:

                while True:
                    
                    # step 1. Create and run container
                    if self.container_id == None:

                            self.container = self._runContainer(self.container_config)        
                            self.container_id = self.container.attrs["Id"]
                    
                    # step 3. Try to get information about container
                    elif bool(self.container_id) and not (container_running == "running"):
                        
                        time.sleep(5)

                        count = count + 1

                        app_internal_port = self.container_config.get('app_internal_port')

                        self.container = self.client.containers.get(container_id=self.container_id)
                        self.container_state = self.container.attrs['State']
                        [con_ext_port] = self.container.ports.get(app_internal_port)
                        con_ext_port = con_ext_port.get('HostPort')
                        container_running = self.container_state.get('Running')
                    
                    # step 2. Send data and get prediction         
                    if bool(container_running):

                        self.service_response = callback(
                            host = self.app_host,
                            port = con_ext_port,
                            settings = self.settings, 
                            history = self.history,
                            future = self.future
                            )
                        
                        if self.service_response:

                            forecast_data = self.service_response.get('prediction')
                            self.stop = str(datetime.datetime.now())
                            
                            return {
                                    "metadata": {
                                        "containerId": self.container_id,
                                        "point": self.mpoint,
                                        "start_time": self.start,
                                        "finish_time": self.stop
                                        },
                                    'prediction': forecast_data
                                    }                           

                    elif count >= 5:

                        logger.warning('Max retries exeeded')
                        break
                
            except (APIError, DockerException) as exc:
                
                logger.error(exc)
                raise exc
         
            finally:
                    
                # step 4.1 stop and remove a container
                self._removeContainer(self.container_id)
            
            
    def mainFlow(self, data):

        self.metadata = data.get('metadata')
        self.settings = self.metadata.get('settings')
        self.mpoint = self.metadata.get('point')
        self.mtype = self.metadata.get('type')

        self.history = data.get('history')
        self.future = data.get('future')

        response = None

        self.future_columns = []     

        if self.mpoint == None:
            raise RuntimeError  

        if (len(self.history[0]) - 1) != len(self.future[0]):
            logger.error('History data columns must be one more of regressor data')
            raise ValueError

        if self.mtype == 'prophet':

            self.container_config = self.prophet_conatiner_config

            return self._steps(callback = handleProphetService)
        
        if self.mtype == 'tensorflow':

            return response
        
        else: 
        
            raise ValueError        


def data_checkout(sample, columns=None):

    _data = pd.DataFrame(data=sample, columns=columns)

    if len(_data) == 0:
        logger.info('Dataset cannot be empty')
        raise ValueError

    data = _data.to_dict()

    return data

def call_web_service(host, port, payload=None):

    try:

        headers = {"Content-Type": "application/json"}

        body = json.dumps(payload)

        url = f'http://{host}:{port}/action'

        response = requests.request("POST", url, headers=headers, data=body)

        return eval(response.text)

    except (ConnectionError) as exp:

        logger.error(exp)
        raise exp


def handleProphetService(host, port, **kwargs):

        future_data = kwargs.get('future')
        history_data = kwargs.get('history')

        settings=kwargs.get('settings')

        history_data_columns = ['ds','y']
        future_data_columns = ['ds']

        len_of_future_data = len(future_data[0])

        for index in range(0, len_of_future_data):
            
            if index > 0:
                regressor_id = f'x{index}'
                history_data_columns.append(regressor_id)
                future_data_columns.append(regressor_id)

        history = data_checkout(
            sample=history_data,
            columns=history_data_columns

        )
        
        future = data_checkout(
            sample=future_data,
            columns=future_data_columns

        )

        payload = {
                'settings': settings,
                'history': history,
                'future': future
            }
        
        response = call_web_service(host, port, payload)

        return response




class Test(object):
    
    def __init__(self):

        self.start = None
        self.finish = None

        self.config = None
        self.type = None
        self.mpoint = None

        self.response = None
        self.dataset = {}

        self.time_steps = None
        self.time_freq = None

        self.regressor = None
        self.settings = None
    
    #Only for test.
    def job(self, data):

        self.config = data['config']
        self.dataset = data['data']
        # self.regressor = data['regressor']
        self.time_steps = self.config['time_steps']
        self.time_freq = self.config['time_freq']
        self.rolling_window = self.config['rolling']

        hist_test_df = pd.DataFrame(self.dataset, columns=['ds','y','x'])
        hist_test_df['ds'] = pd.to_datetime(hist_test_df['ds'], unit='ms')

        hist_test_df = hist_test_df.set_index(['ds'])
        hist_test_df = hist_test_df.astype('float')

        last_date = hist_test_df.index[-1:]
        time_freq = self.time_freq

        [date_year] = last_date.year.tolist()
        [date_month] = last_date.month.tolist()
        [date_day] = last_date.day.tolist()
        [date_hour] = last_date.hour.tolist()

        date_time = datetime.datetime(date_year, \
                month=date_month, \
                day=date_day, \
                hour=date_hour, \
                minute=0)

        dtr = pd.date_range(start=date_time, periods=len(hist_test_df), freq=time_freq)
        predict_test_df = pd.DataFrame({'date_time':dtr})

        transform = pd.DataFrame(data=hist_test_df['y'].values, columns=['y'])
        transform = transform.iloc[::-1]
        # logger.info(transform)
        transform = transform.rolling(window=self.rolling_window).sum()/self.rolling_window
        # logger.info(transform)
        transform = transform.iloc[::-1]
        # logger.info(transform)
        transform.fillna(method='ffill', inplace=True)
        # logger.info(transform)
        answer = transform['y'].values.tolist()
        self.response = {"predictions": answer[:47]}
        
        return self.response