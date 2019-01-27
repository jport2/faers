import pandas as pd
import numpy as np
import os
input = pd.read_csv("inputweightvectors.txt", skiprows=1, sep="|", header=None, error_bad_lines=False)
input2 = np.transpose(input) # transpose to make inputs first row
input2.columns = input2.iloc[0] # make first row columns
input2 = input2.reindex(input2.index.drop(0))

output = pd.read_csv("outputweightvectors.txt", skiprows=1, sep="|", header=None, error_bad_lines=False)
output2 = np.transpose(output) # transpose to make inputs first row
output2.columns = output2.iloc[0] # make first row columns
output2 = output2.reindex(output2.index.drop(0))

os.chdir("../../../../../jportanova/Downloads")
ryan = pd.read_csv("40264_2013_97_MOESM1_ESM.csv")
ryan['Drug Concept Name'] = ryan['Drug Concept Name'].str.lower()
drugs = ryan.ix[:,3].unique()


outcomes = [">acute_myocardial_infarction",">gastrointestinal_haemorrhage",">acute_hepatic_failure",">renal_failure_acute"]


output3 = np.transpose(output2)
output3.index = output3.index.str.lower()
output3dep = output3.loc[outcomes]
output3dep = np.transpose(output3dep)

input2.columns = input2.columns.str.lower()
input3 = np.transpose(input2)
input3dep = input3.ix[drugs]
input3dep = np.transpose(input3dep)


df = pd.DataFrame()
for i in range(0, input3dep.shape[1],1):
    for j in range(0, output3dep.shape[1],1):
        sigscalar = 1/(1 + np.exp(-np.dot(input3dep.ix[:,i], output3dep.ix[:,j])))
        cosinesim = np.dot(input3dep.ix[:,i], output3dep.ix[:,j])/(np.linalg.norm(input3dep.ix[:,i])*np.linalg.norm(output3dep.ix[:,j]))
        sigcosinesim = 1/(1+np.exp(-cosinesim))
        df = df.append({"Drug":output3dep.columns[j],"Outcome":input3dep.columns[i], "Score":sigscalar,"CosScore":sigcosinesim}, ignore_index=True)

ryan['drugoutcome'] = ">" + ryan.ix[:,1] + ryan.ix[:,3]
df['drugoutcome'] = df.ix[:,1] + df.ix[:,2]

# add _ to ryan
ryan = ryan.replace(" ", "_", regex=True)
# lower drugoutcome variable
df['drugoutcome'] = df['drugoutcome'].str.lower()
ryan['drugoutcome'] = ryan['drugoutcome'].str.lower()

# change outcomes in drug outcome for df
df = df.replace("gastrointestinal", "gi", regex=True)
df = df.replace("haemorrhage", "bleed", regex=True)
df = df.replace("acute_hepatic_failure", "acute_liver_injury", regex=True)
df = df.replace("renal_failure_acute", "acute_kidney_injury", regex=True)
# left join ryan and df

rocdf = pd.merge(ryan, df, how="inner", on="drugoutcome")
rocdf.dropna(subset=['CosScore'],inplace=True) # drop nan's from Score column

from sklearn.metrics import roc_curve, auc
fpr = dict()
tpr = dict()
roc_auc = dict()

fpr, tpr, _ = roc_curve(rocdf['Ground Truth (1-postive control; 0-negative control)'], rocdf['Score'])
roc_auc = auc(fpr, tpr)

import matplotlib.pyplot as plt
%matplotlib inline
plt.figure()
lw = 2
plt.plot(fpr, tpr, color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc)
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic example')
plt.legend(loc="lower right")
plt.show()

print(roc_auc)
