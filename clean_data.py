# Ana Maria Sandoval Jimenez
# Data Cleaning Excercise
# DS-Workflow-SoSe18

# import libraries

import csv
import sys
from pprint import pprint

# open dirty data

from pprint import pprint

# open de dirty data and create a new file with clean data
with open ("dirtydata.csv") as f, open ("cleandata.csv", "w") as out :
    reader = csv.reader(f, delimiter=',')
    writer = csv.writer(out, delimiter=',')
    counter = 0
    oldrow = None 
    for row in reader:
        print(row)
	  # Get values for each variable
        try:
            id, full_name, first_name, last_name, email, gender, age = row
	  # Omit the rows with less then 7 entries
        except ValueError:
            continue
	  # not copy duplicated rows
        if row == oldrow:
            continue
	  # keep header
        if id == 'id':
            pass
	  # each column has a unique id from 1:n
        else:
            id = str(counter)
	  # replace any missing value by NA
        if full_name=='':
            full_name = 'NA'
        if last_name=='': 
            last_name='NA'
        if email=='':
            email='NA'
        if gender=='':
            gender='NA'
        if not age.isdigit():
            age='NA'
        elif int(age) < 0:
            age = str(abs(int(age))),
        oldrow = row
        print(oldrow)
        counter += 1
        writer.writerow([id,full_name, first_name, last_name, email, gender, age])

