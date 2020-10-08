#skript to analyse the CT-Radolan intersection and find out if the cloud typ 
#is significantly linked to precipitation

import os
import pandas as pd
from matplotlib import pyplot as plt
import numpy as np
from scipy import stats
from sklearn.linear_model import LinearRegression
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/02_Data/"

csvDir = "Intersection_CT_RD/"

csvList = os.listdir(workingDir+csvDir)

#------------------------------------------------------------------------------
# 2 LOAD THE DATA AND EXRACT THE NOT 0 VALS
#------------------------------------------------------------------------------
#iterate over the cloud types
ct_vals = ["0","3","4","6","7","8"]
ctList = []
ct0List = []
ct3List = []
ct4List = []
ct6List = []
ct7List = []
ct8List = []

for ctVals in ct_vals:
    for files in csvList:
        csv = pd.read_csv(workingDir+csvDir+files, index_col = 0)
        csv = csv["CT_"+ctVals]
        csv = csv.dropna()
        csv = csv.values.tolist()
        ctList = ctList + csv
        
    if ctVals == "0":
        ct0List = ctList
        ct0List = [x for x in ct0List if x != 0.]
    elif ctVals == "3":
        ct3List = ctList
        ct3List = [x for x in ct3List if x != 0.]
    elif ctVals == "4":
        ct4List = ctList
        ct4List = [x for x in ct4List if x != 0.]
    elif ctVals == "6":
        ct6List = ctList
        ct6List = [x for x in ct6List if x != 0.]
    elif ctVals == "7":
        ct7List = ctList
        ct7List = [x for x in ct7List if x != 0.]
    elif ctVals == "8":
        ct8List = ctList
        ct8List = [x for x in ct8List if x != 0.]
        
    ctList = []
    
del ctList, csvList, ctVals, files
#------------------------------------------------------------------------------
# 3 ANALYSIS
#------------------------------------------------------------------------------

#3.1 Kruskal-Wallis-Test
#-----------------------
stats.kruskal(ct0List,ct3List,ct4List, ct6List, ct7List, ct8List)
#>>stats.kruskal(ct0List,ct3List,ct4List, ct6List, ct7List, ct8List)
#>>KruskalResult(statistic=-2064817882.9559228, pvalue=1.0)

stats.kruskal(ct0List,ct7List,ct8List)

#stats.kruskal(ct0List,ct3List,ct4List,ct6List,ct7List,ct8List)

plt.hist(ct8List)
np.mean(ct8List)
np.std(ct8List)

#3.2 Regression
#--------------
#x = [[0, 1], [5, 1], [15, 2], [25, 5], [35, 11], [45, 15]]


arr = np.array([['one', [1, 2, 3]],['two', [4, 5, 6]]], dtype=np.object)
arr[:,1]
np.array(list(arr[:, 1]), dtype=np.float)

arr = np.array([['one', [1, 2, 3]],['two', [4, 5, 6]]], dtype=np.object)
float_arr = np.vstack(arr[:, 1]).astype(np.float)


x = np.array(ct_vals).reshape(6,1)
y = np.array([np.round_(ct0List[:7940],decimals = 3),
              np.round_(ct3List[:7940],decimals = 3), 
              np.round_(ct4List[:7940],decimals = 3),
              np.round_(ct6List[:7940],decimals = 3),
              np.round_(ct7List[:7940],decimals = 3),
              np.round_(ct8List[:7940],decimals = 3)]).reshape((6,7940))


np.savetxt(workingDir+"x_vals.txt", x, fmt = "%s")
np.savetxt(workingDir+"y_vals.csv", y, delimiter = ",")

model = LinearRegression().fit(x,y)
r2 = model.score(x, y)

#https://realpython.com/linear-regression-in-python/
#https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html#sklearn.linear_model.LinearRegression.fit
#https://stackoverflow.com/questions/48230230/typeerror-mismatch-between-array-dtype-object-and-format-specifier-18e
