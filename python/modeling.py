import pandas as pd
import os
import glob

files = glob.glob('/users/matt/documents/github/citibike/data/citibike*')

#df = pd.concat([pd.read_csv(file) for file in files], axis = 0)

df = pd.read_csv(files[1])
print(df.head)
#df = [pd.read_csv('/users/matt/documents/github/citibike/data/')]




