import pandas as pd
import numpy as np
import os

input = pd.read_csv("inputweightvectors.txt", skiprows=1, sep="|", header=None)
input2 = np.transpose(input) # transpose to make inputs first row
input2.columns = input2.iloc[0] # make first row columns
input2 = input2.reindex(input2.index.drop(0))

output = pd.read_csv("outputweightvectors.txt", skiprows=1, sep="|", header=None, error_bad_lines=False)
output2 = np.transpose(output) # transpose to make inputs first row
output2.columns = output2.iloc[0] # make first row columns
output2 = output2.reindex(output2.index.drop(0))

# to this point we can compute the sigmoid scalar of all of the unique drugs and side effects

# setwd Downloads
# read in ryan
# get list of drugs from ryan
# get lists of outcomes from previous script
# subset to drugs and outcomes

os.chdir("../../../../../../jportanova/Downloads")
ryan = pd.read_csv("40264_2013_97_MOESM1_ESM.csv")
drugs = ryan.ix[:,3].unique()
drugs = ">" + drugs
output3 = np.transpose(output2)
output3dep = output3.loc[drugs]
output3dep = np.transpose(output3dep)

# select columns from input2
outcomes = ["acute_myocardial_infarction", "renal_failure_acute", "acute_kindey_injury",
                                 "acute_hepatic_failure","gastrointestinal_haemorrhage"]

input2.columns = input2.columns.str.lower()
input3 = np.transpose(input2)
input3dep = input3.ix[outcomes]
input3dep = np.transpose(input3dep)


# filter
df = pd.DataFrame()
for i in range(0, input3dep.shape[1],1):
    print(i)
    for j in range(0, output3dep.shape[1],1):
        sigscalar = 1/(1 + np.exp(-np.dot(input3dep.ix[:,i], output3dep.ix[:,j])))
        cosinesim = np.dot(input3dep.ix[:,i], output3dep.ix[:,j])/(np.linalg.norm(input3dep.ix[:,i])*np.linalg.norm(output3dep.ix[:,j]))
        sigcosinesim = 1/(1+np.exp(-cosinesim))
        df = df.append({"Drug":output3dep.columns[j],"Outcome":input3dep.columns[i], "Score":sigscalar,"CosScore":sigcosinesim}, ignore_index=True)

# need to figure out nans, change to lower case,

# ROC Curve
# Change Outcome to the same

# create drugoutcome variable for both data sets
ryan['drugoutcome'] = ryan.ix[:,3] + ryan.ix[:,1]
df['drugoutcome'] = df.ix[:,1] + df.ix[:,2]

# add > to ryan
ryan['drugoutcome'] = ">" + ryan['drugoutcome']

# add _ to ryan
ryan = ryan.replace(" ", "_", regex=True)
# lower drugoutcome variable
df['drugoutcome'] = df['drugoutcome'].str.lower()
ryan['drugoutcome'] = ryan['drugoutcome'].str.lower()

# change outcomes in drug outcome for df
df = df.replace("gastrointestinal", "gi", regex=True)
df = df.replace("haemorrhage", "bleed", regex=True)

# left join ryan and df

rocdf = pd.merge(ryan, df, how="inner", on="drugoutcome")
rocdf.dropna(subset=['Score'],inplace=True) # drop nan's from Score column

from sklearn.metrics import roc_curve, auc
fpr = dict()
tpr = dict()
roc_auc = dict()

rocdf2 = rocdf.groupby(['drugoutcome'])['Score'].max()
rocdf2['Ground Truth (1-postive control; 0-negative control)'] = pd.to_numeric(rocdf2['Ground Truth (1-postive control; 0-negative control)'])
rocdf2.ix[:,4] = pd.to_numeric(rocdf2.ix[:,4])

rocdf2['CosScore'] = pd.to_numeric(rocdf2['CosScore'])
fpr, tpr, _ = roc_curve(rocdf2['Ground Truth (1-postive control; 0-negative control)'], rocdf2['Score'])
roc_auc = auc(fpr, tpr)
