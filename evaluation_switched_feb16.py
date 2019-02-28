import pandas as pd
import numpy as np
import os

incoming_vectors = pd.read_csv('inputweightvectors.txt',skiprows=1,sep='|',header=None)
input_vectors = incoming_vectors.set_index(0).T.to_dict('list')

outgoing_vectors = pd.read_csv('outputweightvectors.txt',skiprows=1,sep='|',header=None)
output_vectors = outgoing_vectors.set_index(0).T.to_dict('list')

os.chdir("/home/jportanova/Downloads")
ryan = pd.read_csv("cleanRyan.csv",skiprows=0,header=None)
ryan.ix[:,1]=">"+ryan.ix[:,1]
print(ryan.head())

df = pd.DataFrame()
for i in range(0,len(ryan.ix[:,1]),1):
    ade=ryan.ix[i,0]
    drug=ryan.ix[i,1]
    if (drug in output_vectors):
        label=ryan.ix[i,2]
        sigscalar = 1 / (1 + np.exp(-np.dot(input_vectors[ade], output_vectors[drug])))
        cosine = np.dot(input_vectors[ade], output_vectors[drug])/(np.linalg.norm(input_vectors[ade])*np.linalg.norm(output_vectors[drug]))
        df = df.append({"Drug":drug,"ADE":ade,"label":label,"cosine":cosine,"sigscalar":sigscalar}, ignore_index=True)

print(df.head())


df.dropna(subset=['label'],inplace=True) # drop nan's
#df.dropna(subset=['cosine'],inplace=True) # drop nan's

# read in sdoct
# rename columns
# make drug side effect column
# lower case and >
# merge with rocdf

# make multiple ROC curve
from sklearn.metrics import roc_curve, auc
fpr = dict()
tpr = dict()
roc_auc = dict()

labels=np.array(pd.to_numeric(df['label']))
scores=np.array(df['cosine'])

print(labels)

# rocdf2['Ground Truth (1-postive control; 0-negative control)'] = pd.to_numeric(rocdf2['Ground Truth (1-postive control; 0-negative control)'])
#rocdf.ix[:,4] = pd.to_numeric(rocdf.ix[:,4])

#rocdf2['CosScore'] = pd.to_numeric(rocdf['CosScore'])
fpr, tpr, _ = roc_curve(labels,scores)
roc_auc = auc(fpr, tpr)

print(roc_auc)
