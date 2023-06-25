# -*- coding: utf-8 -*-
"""
Created on Wed Jun  7 13:56:35 2023

@author: guoha
"""
import scprep
import pandas as pd
data=scprep.io.load_csv('./testdata.csv',cell_axis='column',gene_names=True,cell_names=True)

# print(data)
# print('#########')
# print(data.head())
# print('#########')
# print(data[0])
# data['TFAP2D']


dremi=scprep.stats.knnDREMI(data['TFAP2D'],data['IL1B'],plot=False)
print(dremi)

genelist=data.columns[0:12]
print(genelist)
results=[]

df=pd.DataFrame()
for i in genelist:
    dremi=scprep.stats.knnDREMI(data['TFAP2D'],data[i])
    # print(dremi)
    df=df.append(pd.DataFrame([[dremi]],index=[i]))
df.columns=['TFAP2D']
df

def calculate_knnDREMI(data,geneX,geneY):
    ['''
     calculate_knnDREMI used geneA and some gene list (geneY)
     ''']
    df=pd.DataFrame()
    for i in geneY:
        dremi=scprep.stats.knnDREMI(data[geneX],data[i])
        # print(dremi)
        df=df.append(pd.DataFrame([[dremi]],index=[i]))
    df.columns=[geneX]
    return(df)



data=scprep.io.load_csv('./testdata.csv',cell_axis='column',gene_names=True,cell_names=True)
genelist=data.columns[0:12]
results=calculate_knnDREMI(data, "TFAP2D", genelist)
results
