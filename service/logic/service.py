# import pandas as pd   # must be replaced with internal python tools!
# import datetime
# from docker import DockerClient
# from docker.errors import DockerException, APIError
# from config.logger import logger
# import os
# import time
# import json
# import requests

# # from interface.interface import DockerEngineService, ProphetServiceAPI, TensorFlowServingService


# class Service():

#     def __init__(self, config):

#         self.docker_engine_address = config.get('docker_engine_address')

#         self.docker_container_host_ip = config.get('docker_container_host_ip')

#         try:
#             self.docker_api_client = DockerEngineController(base_url=self.docker_engine_address, timeout=10)
#         except (APIError, DockerException) as exc:
#             logger.error(f'Cannot connect to Docker API due to {0}', exc)
        
#         # self.service_ip_address, _ = self.docker_address.split(':')

#         self.image = config.get('prophet_service_image')
#         self.path_to_source = config.get('path_to_source')

#         self.prophet_source = config.get('path_to_source')
#         self.prophet_destination = '/application'

#     def _states(self, callback, payload):

#             try:

#                 # step 1. Create and run container 
#                 self.docker_api_client.create_container(
#                     port = None,
#                     image = self.image,
#                     source = self.source,
#                     destination = self.destination
#                     )

#                 time.sleep(5)

#                 # step 2. Try to get information about container
#                 _, self.container_state, self.container_port = self.docker_api_client.checkout_container_status()
                
#                 # step 3. Send data and get prediction          
#                 if self.container_state == "running":

#                     self.service_response, _ = callback( 
#                         container_port=self.container_state, 
#                         payload=payload
#                         ) 
                    
#             except Exception as e:
            
#                 logger.error(e)
            
#             finally:
                
#                 # step 4.1 stop container
#                 try:
#                     self.docker_api_client.stop_container()
#                 except Exception as e:
#                     logger.error(e)  
                
#                 # step 4.2 delete container
#                 try:
#                     self.docker_api_client.delete_container()
#                 except Exception as e:
#                     logger.error(e)
                
#             self.response = {
#                             "metadata": {
#                                 "containerId": self.container_id,
#                                 "point": self.mpoint,
#                                 "start_time": self.start,
#                                 "finish_time": self.finish
#                                 }
#                             }        
#             return self.response

#     def _create_df(self, sample, columns=None):

#         _data = pd.DataFrame(data=sample, columns=columns)

#         if len(_data) == 0:
#             logger.info('Dataset cannot be empty')
#             raise ValueError
#         else:       

#             _data[_data.columns[0]] = pd.to_datetime(_data[_data.columns[0]], unit='ms')
#             _data.reset_index(inplace=True, drop=True)

#             return _data
    
#     def _prepare_data_prophet(self, history_data, future_data):

#         history_data_columns = ['ds','y']
#         future_data_columns = ['ds']

#         len_of_future_data = len(future_data[0])

#         for index in range(0, len_of_future_data):
            
#             if index > 0:
#                 regressor_id = f'x{index}'
#                 history_data_columns.append(regressor_id)
#                 future_data_columns.append(regressor_id)

#         df_hist = self._create_df(
#             sample=history_data,
#             columns=history_data_columns

#         )
        
#         df_future = self._create_df(
#             sample=future_data,
#             columns=future_data_columns

#         )       
#         return df_hist, df_future

#     def _call_prophet_service(self, container_port=None, payload=None):
        
#         self.start = str(datetime.datetime.now())
#         host = self.docker_container_host_ip

#         try:

#             headers = {"Content-Type": "application/json"}

#             body = json.dumps(payload)

#             url = f"http://{host}:{container_port}/action"

#             response = requests.request("POST", url, headers=headers, data=body)

#             response = eval(response.text)
#             response_status_code = response.status_code

#         except Exception as error:
#             logger.error(error)
#             response_status_code = 500
#         finally:
#             self.finish = str(datetime.datetime.now())
        
#         self.total_time = self.start - self.finish

#         return response, response_status_code

#     def call_prediction(self, data):

#         self.metadata = data.get('metadata')
#         self.settings = self.metadata.get('settings')
#         self.mpoint = self.metadata.get('point')
#         self.mtype = self.metadata.get('type')

#         history = data.get('history')
#         future = data.get('future')
#         error = {}
#         response = None

#         response_status_code = 200
#         response_text = ''

#         self.future_columns = [] 

#         if self.mpoint.find('/') != -1:
#             self.mpoint, self.version = self.mpoint.split("/")

#         if self.mpoint == None:
#             raise ValueError  

#         if (len(history[0]) - 1) != len(future[0]):
#             logger.error('History data columns must be one more of regressor data')
#             raise ValueError

#         # if (self.service_addr or self.rest_api_port) == None:
#         #     raise ValueError

#         self.source = self.prophet_source
#         self.destination = self.prophet_destination
        
#         if self.mtype == 'prophet':

#             df_hist, df_future = self._prepare_data_prophet(history, future)

#             payload = {
#                 "settings": self.settings,
#                 'history': df_hist.to_dict(),
#                 'future': df_future.to_dict()
#             }

#             response = self._states(callback=self._call_prophet_service, payload=payload)
        
#         elif self.mtype == 'tensorflow':
#             pass
#         else: 
#             raise ValueError        
        
#         return response, response_status_code

# class Test(object):
    
#     def __init__(self):

#         self.start = None
#         self.finish = None

#         self.config = None
#         self.type = None
#         self.mpoint = None

#         self.response = None
#         self.dataset = {}

#         self.time_steps = None
#         self.time_freq = None

#         self.regressor = None
#         self.settings = None
    
#     #Only for test.
#     def job(self, data):

#         self.config = data['config']
#         self.dataset = data['data']
#         # self.regressor = data['regressor']
#         self.time_steps = self.config['time_steps']
#         self.time_freq = self.config['time_freq']
#         self.rolling_window = self.config['rolling']

#         hist_test_df = pd.DataFrame(self.dataset, columns=['ds','y','x'])
#         hist_test_df['ds'] = pd.to_datetime(hist_test_df['ds'], unit='ms')

#         hist_test_df = hist_test_df.set_index(['ds'])
#         hist_test_df = hist_test_df.astype('float')

#         last_date = hist_test_df.index[-1:]
#         time_freq = self.time_freq

#         [date_year] = last_date.year.tolist()
#         [date_month] = last_date.month.tolist()
#         [date_day] = last_date.day.tolist()
#         [date_hour] = last_date.hour.tolist()

#         date_time = datetime.datetime(date_year, \
#                 month=date_month, \
#                 day=date_day, \
#                 hour=date_hour, \
#                 minute=0)

#         dtr = pd.date_range(start=date_time, periods=len(hist_test_df), freq=time_freq)
#         predict_test_df = pd.DataFrame({'date_time':dtr})

#         transform = pd.DataFrame(data=hist_test_df['y'].values, columns=['y'])
#         transform = transform.iloc[::-1]
#         # logger.info(transform)
#         transform = transform.rolling(window=self.rolling_window).sum()/self.rolling_window
#         # logger.info(transform)
#         transform = transform.iloc[::-1]
#         # logger.info(transform)
#         transform.fillna(method='ffill', inplace=True)
#         # logger.info(transform)
#         answer = transform['y'].values.tolist()
#         self.response = {"predictions": answer[:47]}
        
#         return self.response

# class DockerEngineController(DockerClient):   
     
#     def checkout_container_status(self):
        
#         resp = self.containers.get(container_id=self.container_id)
        
#         if bool(resp):

#             if "Id" in resp.attrs:
#                 self.container_id = resp.attrs["Id"]
#             if "State" in resp.attrs:
#                 self.container_state = resp.attrs["State"]
#             else:
#                 raise 
            
#             # if bool(resp.ports):

#                 # self.container_port = int(resp.ports.["HostPort"])
#                 # self.container_port = resp.ports

#         return self.container_id, self.container_state, resp.ports

#     def create_container(self, image, port, source, destination):

#         print(port)

#         resp = self.containers.run(
#             image=image,
#             ports= port,
#             volumes={source: {"bind": destination, "mode": "rw"}},
#             # environment=self.environment,
#             detach=True
#         )

#         if bool(resp):

#             if "Id" in resp.attrs:
#                 self.container_id = resp.attrs["Id"]
#             if "State" in resp.attrs:
#                 self.container_state = resp.attrs["State"]
#             if bool(resp.ports):
#                 self.container_ports = resp.ports

#     def stop_container(self):
#         self.container = self.containers.get(container_id=self.container_id)
#         self.container.stop()

#     def delete_container(self):
#         self.container = self.client.containers.get(self.container_id)
#         self.container.remove()

#     def available_items(self):
#         pass

#     def update_image(self, repository, tag):
#         status = self.client.images.pull(repository=repository, tag=tag, all_tags=False)
#         print(status)