"""
    Class TensorFlow:
    

"""
import pandas as pd
import numpy as np

from sklearn.preprocessing import MinMaxScaler

from dotenv import load_dotenv
from pathlib import Path
import pandas as pd
import json

import mlflow
import tensorflow as tf
from tensorflow import keras

from helper import get_logger, LOG_LEVEL
logger = get_logger(__name__, loglevel=LOG_LEVEL)

logger.info(f'LOG_LEVEL: {LOG_LEVEL}')

class Model(object):

    def __init__(self, tracking_server):
        self.model = None
        self.output_fields = None
        try:
            mlflow.set_tracking_uri(tracking_server)
        except:
            logger.error(
            """Couldn't connect to remote MLFLOW tracking server""")

    def run(self, data):

        metadata = data.get('metadata')
        self.model_uri = data.get('model_uri')
        dataset = data.get('dataset')
        period = data.get('period')

        if self.model_uri:
            self.model = mlflow.tensorflow.load_model(self.model_uri)
        
        _dataset = self.prepare_dataset(dataset)
        predict = self.model.predict(_dataset)

        _result = predict[self.output_fields].values.tolist()

        return {
            "prediction": _result,
            "anomalies": None, 
            "error": False,
            "model_uri": self.model_uri
            }

    def prepare_dataset(self, dataset):

        # Load dataset as pandas dataframe
        df = pd.DataFrame(dataset)
        # Replace Nan with previouse value
        df.fillna( method='ffill', inplace=True )
        # Replace 0 with previouse value
        df.replace(to_replace=0, method='ffill', inplace=True )
        df.set_index(df.columns[0], inplace=True)
        df['dt'] = pd.to_datetime(df.index, unit='ms')
        
        df['of_day'] = df['dt'].dt.dayofweek

        # of week
        df['of_week'] = df['dt'].dt.week

        # of month
        df['of_month'] = df['dt'].dt.month

        df.drop('dt', inplace=True, axis=1)

        # Normalize features 
        scaler = MinMaxScaler(feature_range=(0, 1))
        series = scaler.fit_transform(df.values) 

        return series   