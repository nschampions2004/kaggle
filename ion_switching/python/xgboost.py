from pathlib import Path
import pandas as pd
import numpy as np
from utils import train_file, test_file

train_data = pd.read_csv(train_file)
train_data.index = ((train_data.time * 10_000) - 1).values
train_data['batch'] = train_data.index // 50_000

train_data = train_data \
    .assign(batch_index = train_data.index  - (train_data['batch'] * 50_000)) \
    .assign(batch_slices = lambda x: x.batch_index // 5000)

train_data.head()

train_data['batch_slices2'] = train_data.apply(lambda r: '_'.join([str(r['batch']).zfill(3),
    str(r['batch_slices']).zfill(3)]), axis=1)

for c in ['batch','batch_slices2']:
    d = {}
    d['mean'+c] = df.groupby([c])['signal'].mean()
    d['median'+c] = df.groupby([c])['signal'].median()
    d['max'+c] = df.groupby([c])['signal'].max()
    d['min'+c] = df.groupby([c])['signal'].min()
    d['std'+c] = df.groupby([c])['signal'].std()
    d['mean_abs_chg'+c] = df.groupby([c])['signal'].apply(lambda x: np.mean(np.abs(np.diff(x))))
    d['abs_max'+c] = df.groupby([c])['signal'].apply(lambda x: np.max(np.abs(x)))
    d['abs_min'+c] = df.groupby([c])['signal'].apply(lambda x: np.min(np.abs(x)))
