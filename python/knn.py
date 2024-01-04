# -*- coding: utf-8 -*-
"""
this script used to calculate knn sorce
Created on Sun Jun 25 15:21:57 2023

@author: guoha
"""
import scprep  # knnDREMI package
import pandas as pd
# import openpyxl


def calculate_knnDREMI(data, geneX, geneY):
    ['''
     calculate_knnDREMI used geneA and some gene list (geneY)
     ''']
    df = pd.DataFrame()
    for i in geneY:
        dremi = scprep.stats.knnDREMI(data[geneX], data[i])
        # print(dremi)
        df = df.append(pd.DataFrame([[dremi]], index=[i]))
    df.columns = [geneX]
    return(df)


data = scprep.io.load_csv(
    './testdata.csv', cell_axis='column', gene_names=True, cell_names=True)
genelist = data.columns
testlist = ["HLA-A", "PRKCH", "IGHA1", "BST2", "AHNAK", "IGKC", "PLA2G4A", "SULF2", "SPARCL1", "IGHG2"]
# results = calculate_knnDREMI(data, "TFAP2D", genelist)
writer =  pd.ExcelWriter(r'knnDREMI_resutls.xlsx')
for gene in testlist:
    results = calculate_knnDREMI(data, gene, genelist)
    results.to_excel(writer,sheet_name=gene)  
writer.close()
