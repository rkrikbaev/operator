"""
    Class Prophet:
    
    Prophet is a procedure for forecasting time series data based on 
    an additive model where non-linear 
    trends are fit with yearly, weekly, and daily seasonality, plus holiday effects. 
    It works best with time series that have strong seasonal effects and several seasons of historical data. 
    Prophet is robust to missing data and shifts in the trend, and typically handles outliers well.

"""

from dotenv import load_dotenv
from pathlib import Path
import pandas as pd
import json

import mlflow
from prophet import Prophet, serialize

from helper import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

dotenv_path = Path('.env')
load_dotenv(dotenv_path=dotenv_path)

ARTIFACT_PATH = 'model'

class Model(object):

    def __init__(self, tracking_server):

        self.output_fields = ['ds', 'yhat']
        # self.model_path = model_path
        self.model = None

        try:
            mlflow.set_tracking_uri(tracking_server)
        except:
            logger.error(
            """Couldn't connect to remote MLFLOW tracking server""")

    def run(self, data):

        regressor_names = data.get('regressor_names')
        self.model_uri = data.get('model_uri')
        self.model_point = data.get('model_point')

        dataset = data.get('dataset')
        logger.debug(dataset)
        period = data.get('period')
        logger.debug(period)
        
        if regressor_names == None:
            regressor_names = []

        df_dataset = self._create_df(
            data=dataset,
            columns=['ds', 'y'] + regressor_names
            )

        df_period = self._create_df(
            data=period,
            columns=['ds'] + regressor_names
    )

        if self.model_uri:
            try:
                self.model = mlflow.prophet.load_model(self.model_uri)
            except Exception as exc:
                logger.error(exc)
        
        else:
            self.model_uri, self.model = self._fit_model(
                data=df_dataset,
                experiment=self.model_point, 
                settings=json.loads(data.get('model_config'))
                )
        
        if self.model:
            forecast = self.model.predict(df_period)
            forecast['ds'] = forecast['ds'].astype('string')

            logger.debug('Find anomalies')

            anomalies = self._find_anomalies(
                forecast,
                dataset=df_dataset
                )
            
            logger.debug('Filter response')

            filtred_result = forecast[self.output_fields].values.tolist()

            return {
                "prediction": filtred_result,
                "anomalies": anomalies, 
                "error": False,
                "model_uri": self.model_uri
                }

    def _fit_model(self, data, experiment, settings):

        def extract_params(pr_model):
            return {attr: getattr(pr_model, attr) for attr in serialize.SIMPLE_ATTRIBUTES}

        mlflow.set_experiment(experiment)
        experiment = mlflow.get_experiment_by_name(experiment)

        with mlflow.start_run(experiment_id=experiment.experiment_id):

            model = Prophet(
                growth=settings.get('growth'),
                seasonality_mode=settings.get('seasonality_mode'),
                changepoint_prior_scale=settings.get(
                    'changepoint_prior_scale'),
                seasonality_prior_scale=settings.get(
                    'seasonality_prior_scale'),
                interval_width=settings.get('interval_width'),
                daily_seasonality=settings.get('daily_seasonality'),
                weekly_seasonality=settings.get('weekly_seasonality'),
                yearly_seasonality=settings.get('yearly_seasonality')
            )
            seasonality = settings.get('seasonality')
            
            for item in seasonality:
                model.add_seasonality(
                    name=item.get('name'), 
                    period=item.get('period'), 
                    fourier_order=item.get('fourier_order'))
            
            logger.debug('fit data')
            logger.debug(data)
            model.fit(data)

            params = extract_params(model)

            mlflow.prophet.log_model(model, artifact_path=ARTIFACT_PATH)
            mlflow.log_params(params)

            # mlflow.log_metrics(metrics)
            model_uri = mlflow.get_artifact_uri(ARTIFACT_PATH)
            print(f"Model artifact logged to: {model_uri}") 

            return model_uri, model

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