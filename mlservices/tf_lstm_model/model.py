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

    def run( self, dataset, **kwargs ):

        window = kwargs.get('window')
        run_id = kwargs.get('run_id')
        experiment_id = kwargs.get('experiment_id')
        
        
        assert window != None
        assert run_id != None

        uri = f'file://{path_abs}/mlruns/{experiment_id}/{run_id}/mlmodel'

        model = mlflow.tensorflow.load_model(uri)
        
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

        # Normalize features back
        _max = X[X.columns[0]].max()
        _min = X[X.columns[0]].min()

        X_series = np.array(X.values)
        assert X_series.shape[0] == window +1

        # create sclice
        N = X_series.shape[0]
        k = N - window
        X_slice = np.array([range(i, i + window) for i in range(k)])
        X_data = X_series[X_slice,:]

        in_data = X_data[0]
        in_data = np.reshape(in_data, (1, window, X_data.shape[2]))

        predict = model.predict(in_data)[0] * _max + _min
        predict_values = list(predict)

        return {
            "prediction": predict_values,
            "anomalies": None, 
            "error": False,
            "model_uri": self.uri
            } 