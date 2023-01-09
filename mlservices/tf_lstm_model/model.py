"""
    Class TensorFlow:
    

"""

import pandas as pd
import numpy as np

import mlflow
import mlflow.keras

from pathlib import Path

from helper import get_logger, LOG_LEVEL
logger = get_logger(__name__, loglevel=LOG_LEVEL)

logger.info(f'LOG_LEVEL: {LOG_LEVEL}')

path_abs = Path(__file__).parent.absolute()

class Model(object):

    def __init__(self, tracking_server):
        self.model = None

        try:
            mlflow.set_tracking_uri(tracking_server)
        except:
            logger.error(
            """Couldn't connect to remote MLFLOW tracking server""")

    def run(self, dataset, **kwargs):
    
        window = kwargs.get('window')
        model_uri = kwargs.get('uri')

        print("\n**** mlflow.keras.load_model\n")
        model = mlflow.keras.load_model(model_uri)
        print("model:", type(model))
        
        X = self.prepare_dataset(dataset)
        X_series, _min, _max = self.normalize_data(X, column_index=0)

        assert X_series.shape[0] == window +1

        in_data = self.slice_data(X_series, window)

        predict = model.predict(in_data)[0] * _max + _min
        predict_values = list(map(lambda x: float(x), predict))

        return predict_values

    def prepare_dataset( self, dataset ):
        # Convert dataset to pandas DataFrame
        X = pd.DataFrame(dataset)

        # Replace N/A values with previouse value
        X.fillna( method='ffill', inplace=True )
        # Replace 0 with previouse value
        X.replace(to_replace=0, method='ffill', inplace=True )

        X.set_index(X.columns[0], inplace=True)
        X['dt'] = pd.to_datetime(X.index)

        # create additional features from date
        # Day of week
        X['of_day'] = X['dt'].dt.dayofweek
        # of week
        X['of_week'] = X['dt'].dt.week
        # of month
        X['of_month'] = X['dt'].dt.month

        # drop columns
        X.drop('dt', inplace=True, axis=1)  

        return X

    def normalize_data(self, X, column_index):
        # Normalize features 
        _max = X[X.columns[column_index]].max()
        _min = X[X.columns[column_index]].min()

        X_series = ( (np.array(X.values) - _min ) / _max )

        return X_series, _min, _max

    def slice_data(self, X_series, window):
        # create sclice
        N = X_series.shape[0]
        k = N - window
        X_slice = np.array([range(i, i + window) for i in range(k)])
        X_data = X_series[X_slice,:]

        in_data = X_data[0]
        in_data = np.reshape(in_data, (1, window, X_data.shape[2]))

        return in_data