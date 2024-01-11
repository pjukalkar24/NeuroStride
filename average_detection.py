from pathlib import Path
import pandas as pd
import scipy.integrate as spi

import warnings
warnings.simplefilter("ignore", category=Warning)

def average_value(df, col, a, b):
  def f(x, df=df, col=col):
    return df.loc[int(x)][col]

  result, error = spi.quad(f, a, b)
  return result * (1 / (b - a))

def calc_avg(file):
    if Path("sensor_alignment.csv").is_file():
        sensor_df = pd.read_csv("sensor_alignment.csv")
        sensor_df = sensor_df.drop_duplicates()

        for x in sensor_df.columns:
            avg = average_value(sensor_df, x, 0, len(sensor_df))
            file.write(x + " " + str(avg) + '\n')
        print("average values updated.")
    else:
        print(FileNotFoundError)

try:
    with open('average_value.txt', 'r+') as avg_val:
        val = avg_val.readlines()

        if not val:
            calc_avg(avg_val)
        else:
            print(val)

except FileNotFoundError:
    with open('average_value.txt', 'w') as avg_val:
        calc_avg(avg_val)