"""
    Class Prophet:
    
    Prophet is a procedure for forecasting time series data based on 
    an additive model where non-linear 
    trends are fit with yearly, weekly, and daily seasonality, plus holiday effects. 
    It works best with time series that have strong seasonal effects and several seasons of historical data. 
    Prophet is robust to missing data and shifts in the trend, and typically handles outliers well.

"""

from email.policy import default
from xml.sax.handler import feature_namespaces
from dotenv import load_dotenv
from pathlib import Path
import pandas as pd
import json
import os

import mlflow
from prophet import Prophet, serialize

dotenv_path = Path('.env')
load_dotenv(dotenv_path=dotenv_path)

LOG_LEVEL = os.environ.get('LOG_LEVEL')
if LOG_LEVEL==None:
    LOG_LEVEL='INFO'

from helper import get_logger
logger = get_logger(__name__, loglevel=LOG_LEVEL)

logger.info(LOG_LEVEL)

ARTIFACT_PATH = 'model'

class Model(object):

    def __init__(self, tracking_server):
        self.model = None
        try:
            mlflow.set_tracking_uri(tracking_server)
        except:
            logger.error("""Couldn't connect to remote MLFLOW tracking server""")
            raise RuntimeError('Could not connect to remote MLFLOW tracking server')

    def run(self, data):

        try:
            metadata = data.get('metadata')
            self.model_uri = data.get('model_uri')
            dataset = data.get('dataset')
            period = data.get('period')
        except (KeyError, AttributeError) as err:
            logger.error(err)
            raise RuntimeError(err)

        for item in metadata:
            value = metadata[item]
            if isinstance(value, str):
                metadata[item] = json.loads(value)
            elif isinstance(value, list):
                pass
            elif value is None:
                metadata[item] = []
            else:    
                raise TypeError()

        regressor_names = metadata.get('regressor_names')

        df_dataset = self._create_df(
            data=dataset,
            columns = ['ds', 'y'] + regressor_names
            )
        
        df_period = self._create_df(
            data=period,
            columns = ['ds'] + regressor_names
        )

        if self.model_uri:
            try:
                self.model = mlflow.prophet.load_model(self.model_uri)
            except Exception as exc:
                logger.error(exc)
        else:
            settings = data.get('model_config')          
            self.model_uri, self.model = self._fit_model(
                data=df_dataset,
                settings=settings
                )
        
        if self.model:
            forecast = self.model.predict(df_period)

            logger.debug('Find anomalies')

            anomalies = self._find_anomalies(
                forecast,
                dataset=df_dataset
                )

            filter = ['ts', 'yhat'] + metadata.get('model_features')
            logger.debug('Filter value')
            logger.debug(filter)
            forecast['ts'] = forecast[['ds']].apply(lambda x: x[0].timestamp()*1000, axis=1)
            forecast['ts'] = forecast['ts'].astype(int)
            forecast['ds'] = forecast['ds'].astype('string')

            filtred_result = forecast[filter].values.tolist()
            filtred_result = list(map(lambda x: [int(x[0]), x[1]], filtred_result))

            logger.debug('Filter response')
            logger.debug(filtred_result)
            
            return {
                "prediction": filtred_result,
                "anomalies": anomalies, 
                "error": False,
                "model_uri": self.model_uri
                }

    def _fit_model(self, data, settings):

        def extract_params(pr_model):
            return {attr: getattr(pr_model, attr) for attr in serialize.SIMPLE_ATTRIBUTES}
        
        init_settings = None
        seasonality = None       
        
        if type(settings) == 'string':
            _settings = json.loads(settings)
            init_settings = _settings.get('init')
            seasonality = _settings.get('seasonality') 

        if init_settings:
            self.model = Prophet(init_settings)
        else:
            self.model = Prophet()
        
        if seasonality:
            [self.model.add_seasonality(**items) for items in seasonality]
        
        self.model.fit(data)
        params = extract_params(self.model)

        mlflow.prophet.log_model(self.model, artifact_path=ARTIFACT_PATH)
        mlflow.log_params(params)

        # mlflow.log_metrics(metrics)
        model_uri = mlflow.get_artifact_uri(ARTIFACT_PATH)
        print(f"Model artifact logged to: {model_uri}") 

        return model_uri, self.model

    def _create_df(self, data, columns) -> pd.DataFrame:

        df = pd.DataFrame(data, columns=columns)             
        df[df.columns[0]] = pd.to_datetime(
            df[df.columns[0]], unit='ms', utc=False)
        df.reset_index(inplace=True, drop=True)
        
        return df

    def _find_anomalies(self, forecast, dataset) -> list:
        forecast = forecast[['ds', 'yhat_lower', 'yhat_upper', 'yhat']]
        print(forecast)
        print(dataset)
        df1 = dataset.set_index('ds').join(
            forecast.set_index('ds'))
        df1.query('y < yhat_lower or y > yhat_upper', inplace=True)
        if df1.empty:
            return []
        else:
            return df1[['ds', 'y']].values.tolist()

    def _err_handle(self, err):

        text = str(err)
        logger.error(f'Error: {text}')
        raise RuntimeError(f'"error_text": {str(err)}')
