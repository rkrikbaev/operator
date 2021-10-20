# import numpy as np
# import pandas as pd
# import sklearn
# from sklearn.preprocessing import MinMaxScaler


# class Processing():

#     def __init__(self, isdate=False):

#         self.isdate = isdate
#         self.min_value = None
#         self.max_value = None

#     def prepare_data(self, df):

#         df_featers = self._process_features(df)

#         self.min_value = df_featers.min()
#         self.max_value = df_featers.max()

#         # transform to range from -1 to 1
#         df_scaled = (df_featers - self.min_value) / (self.max_value - self.min_value)*2 - 1

#         df_scaled.fillna(value=-1, inplace=True)

#         return df_scaled

#     def invert_response(self, data):

#         income_data = pd.DataFrame(data=data[0].tolist())

#         value_max = income_data.max()
#         value_min = income_data.min()

#         origin_scale_max = self.max_value.values[0]
#         origin_scale_min = self.min_value.values[0]

#         standart_norm_data = (income_data - value_min) / (value_max - value_min)

#         data_inverted = standart_norm_data * (origin_scale_max - origin_scale_min) + origin_scale_min

#         return data_inverted

#     def _process_features(self, df):

#         if self.isdate:

#             holiday_dates = ['-12-31', '-01-01', '-01-02', '-01-07', '-03-08', '-03-21', '-03-22', '-03-23', 
#                  '-05-01', '-05-07', '-05-09', '-07-06', '-08-30', '-12-01', '-12-16', '-12-17']

#             df["ds"] = df.index
#             df["ds"] = pd.to_datetime(df["ds"], unit="ms")

#             years = df["ds"].dt.year
#             holidays = []

#             for year in years.unique():
#                 for date in holiday_dates:
#                     holidays.append(str(year) + date)

#             df["isholidays"] = np.where(df["ds"].isin(holidays), 1, -1)

#             df["day_of_week"] = np.sin(2 * np.pi * df["ds"].dt.dayofweek / 7)
#             # df['month'] = np.sin(2 * np.pi * df['ds'].dt.month / 12)
#             df["hour"] = np.sin(2 * np.pi * df["ds"].dt.hour / 24)

#             df["days_in_month"] = df["ds"].dt.daysinmonth
#             df["day_of_month"] = np.sin(
#                 2 * np.pi * df["ds"].dt.day / df["days_in_month"]
#             )

#             df.drop(columns=["ds", "days_in_month"], inplace=True)

#         return df
