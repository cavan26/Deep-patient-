
import pandas as pd
import numpy as np
from itertools import groupby
from datetime import datetime


REPO_PATH = '/Users/camille.vanassel/Data Intelligence/Deep Patient '
METADATA_PATH = '/Users/camille.vanassel/Data Intelligence/Deep Patient /data'

codes = pd.read_csv(METADATA_PATH + '/Medcode_short.csv')
patients = pd.read_csv(METADATA_PATH + '/Patients_short.csv')
orders = pd.read_csv(METADATA_PATH + '/Therapy_short.csv')

# Fetch descriptors
allcodes = np.unique(codes['medcode'])
allpatients = np.unique(codes['combid'])

print(orders.keys())

patients = patients[patients['combid'].isin(allpatients)]
orders = orders[orders['combid'].isin(allpatients)]
# Number of events
nb_events = len(codes['combid'])
nb_orders = len(orders['combid'])

# Compute the age at event date, in days
nb_of_event_per_patient = [sum(1 for _ in group) for _, group in groupby(codes['combid'])]
nb_of_event_per_patient_cumsum = np.cumsum(nb_of_event_per_patient)
FMT = '%Y-%m-%d'
age_at_visit = []
age_at_start = []
age_at_order = []

for i in range(0, nb_events):
    age_at_visit.append((datetime.strptime(codes['sysdate'][i], FMT) - datetime.strptime(patients['yob'][list(patients['combid']).index(codes['combid'][i])], FMT)).days)
    age_at_start.append((datetime.strptime(codes['eventdate'][i], FMT) - datetime.strptime(patients['yob'][list(patients['combid']).index(codes['combid'][i])], FMT)).days)
codes['age_at_visit_in_days'] = age_at_visit
codes['age_at_start_time_in_days'] = age_at_start

for i in range(0, nb_orders):
    age_at_order.append((datetime.strptime(orders['prscdate'][i], FMT) - datetime.strptime(patients['yob'][list(patients['combid']).index(orders['combid'][i])], FMT)).days)
orders['age_at_start_time_in_days'] = age_at_order

# Filter out the rows with events happening before the patient birth
codes = codes.drop(codes[codes.age_at_start_time_in_days < 0].index)

# Filter years according to cutting point.
codes['year'] = [int(date[0:4]) for date in codes['eventdate']]
codes2014 = codes[codes['year'] == 2014]
codes = codes[codes['year'] < 2014]

orders['year'] = [int(date[0:4]) for date in orders['prscdate']]
orders2014 = orders[orders['year'] == 2014]
orders = orders[orders['year'] < 2014]


# Count codes before and after the splitting point.
codes_group = codes.groupby(['combid','medcode']).aggregate({'category': 'count', 'age_at_start_time_in_days': 'mean'})
codes2014_group = codes2014.groupby(['combid','medcode']).aggregate({'category': 'count', 'age_at_start_time_in_days':'mean'})
orders_group = orders.groupby(['combid', 'drugcode']).aggregate({'prscdate':'count', 'age_at_start_time_in_days': 'mean'})
orders2014_group = orders2014.groupby(['combid', 'drugcode']).aggregate({'prscdate':'count', 'age_at_start_time_in_days': 'mean'})

# import pdb; pdb.set_trace()

# Remove patients with too few records before 2014
ids = np.unique(codes_group.index.levels[0])
Index = list(pd.Index(ids))
totalCounts = pd.DataFrame(np.repeat(0, len(ids), axis=0), index=Index)

by_patient = orders.groupby('combid')
counts = by_patient.aggregate({'drugcode':'count'})
print(counts)

for idx in Index:
    if idx in orders_group.index.levels[0]:
        totalCounts.loc[idx] += counts['drugcode'].loc[idx]

by_patient = codes.groupby(['combid'])
counts = by_patient.aggregate({'medcode':'count'})

for idx in Index:
    totalCounts.loc[idx] += counts['medcode'].loc[idx]

import pdb; pdb.set_trace()

# Patient IDs to keep
ids = np.asarray(ids)
ids = ids[totalCounts[0].values >= 5]
codes2014 = codes2014.loc[codes2014['combid'].isin(ids)]

# Remove codes appearing in less than 5 patients or more than 80% of patients.
codecounts = codes.groupby('medcode').count()
code_to_keep = codecounts.index[5 < codecounts['medcode'].values < 0.8*len(ids)]

counts = codes.groupby('prscdate').count()
code_to_keep = counts.index[5 < counts['prscdate'] < 0.8*len(ids)]

# Sample test patients
import pdb
pdb.set_trace()

test_patients = np.unique(codes2014.index.levels[0])
test_patients = test_patients
