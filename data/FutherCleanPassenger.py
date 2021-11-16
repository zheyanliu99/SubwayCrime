#%%  packages

import pandas as pd
import numpy as np


# %%
df = pd.read_csv('passenger_final.csv')
df['date'] = pd.to_datetime(df['date'])

date_list = df['date']
time_list = df['time']
date_list_new = []
time_list_new = []
for date, time in zip(date_list, time_list):
    if time == 0:
        time_list_new.append(86400)
        date_list_new.append(date -  pd.Timedelta('1 days'))
    else:
        time_list_new.append(time)
        date_list_new.append(date)

df['date'] = date_list_new
df['time'] = time_list_new
df['weekday'] = df['date'].dt.dayofweek

# %%
def impute_diff(df, target, weeknum, shreshold):
    df[target+'_imputed'] = df[target]
    for index, diff in enumerate(df[target]):
        if index % 10000 == 0:
            print(index)
        if diff < 0 or diff > shreshold:
            missing_time = df['weekday'][index]
            # set 30
            df_sub = df[index-30:index]
            imputed = np.mean(df_sub[df_sub['weekday'] == missing_time][target+'_imputed'][-weeknum:])
            df[target+'_imputed'][index] = imputed
    return df


df_final = impute_diff(df, 'entry_diff', 2, 100000)
df_final = impute_diff(df_final, 'exit_diff', 2, 100000)

df_final.drop('time_period').to_csv('passenger_imputed.csv',index=False)
