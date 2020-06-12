import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
import os
import glob
from pathlib import Path

ROOT_DIR = Path(os.path.dirname(os.path.abspath(__file__))).parents[0] # This is your Project Root

print(ROOT_DIR)
files = glob.glob(os.path.join(ROOT_DIR, 'data/citibike*'))

df = pd.concat([pd.read_csv(file) for file in files], axis = 0)

df = (
    df
    .assign(date=lambda x: pd.to_datetime(x.starttime.str.slice(0, 10), format = "%Y-%m-%d"))
    .groupby('date')
    .agg('size')
    .to_frame()
    .rename({0: 'NumTrips'}, axis='columns')
    .reset_index(drop = False)
    .assign(dow=lambda x: x.date.dt.day_name())
    .assign(weekend=lambda x: np.where(x.dow.isin(['Saturday', 'Sunday']), 'Weekend', 'Weekday'))
)

print(df)
#df = [pd.read_csv('/users/matt/documents/github/citibike/data/')]

mod = RandomForestRegressor().fit(df.dow, df.NumTrips)

print(mod)


