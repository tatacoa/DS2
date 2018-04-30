#Ana Maria Sandoval Jimenez
# Exercise 1: Data Cleaning with Python

# load/open file

import pandas as pd
import csv
from pprint import pprint
import sys

with open ('/Users/tata/DS2/dirty_data.csv') as file:
    reader = csv.reader(file,  delimiter = ',')
    for row in file:
        print(row)


# open with pandas

df = pd.read_csv('/Users/tata/DS2/dirty_data.csv')
print(df)
