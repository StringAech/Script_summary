# -*- coding: utf-8 -*-
"""
Created on Fri Jun  2 14:35:19 2023

@author: guoha
"""

import os
import gzip

os.getcwd()
os.chdir("E:\\yuceguohao\\code\\python")
os.getcwd()

file = gzip.open("./mRNA.ali.human.20230531_mRNA_result.tar.gz",'rb')
print(file)
file.close()



import gzip
import os


import tarfile
