import os
import pandas as pd
import numpy as np
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/02_Data/"

csvDir = "Intersection_CT_RD/"

csvList = os.listdir(workingDir+csvDir)

#------------------------------------------------------------------------------
# 2 LOAD THE DATA AND EXRACT THE NOT 0 VALS
#------------------------------------------------------------------------------
#overall df
ct_vals = ["CT_0","CT_3","CT_4","CT_6","CT_7","CT_8"]

df_all = pd.DataFrame(columns = ["cloudType", "value"])

for files in csvList:
    #load csv file of one time step
    csv = pd.read_csv(workingDir+csvDir+files, index_col = 0)
    #iterate over all columns
    for col in ct_vals:
        csv_temp = csv[col]
        #remove 0 values
        csv_temp = [x for x in csv_temp if x != 0.]
        csv_temp = csv_temp + list(np.repeat(np.nan, 28000-len(csv_temp)))
        df_temp = pd.DataFrame(columns = ["cloudType", "value"], index = np.arange(1,28001))
        df_temp["cloudType"] = np.repeat(col, 28000)
        df_temp["value"] = csv_temp
        #remove NaN
        df_temp = df_temp.loc[pd.notnull(df_temp.value)]
        #add the col to df
        frame = [df_all, df_temp]
        df_all = pd.concat(frame)

#------------------------------------------------------------------------------
# 3 WRITE TO .CSV
#------------------------------------------------------------------------------
    
df_all.to_csv(workingDir+"dataOneDay.csv", sep = ",")
        
    