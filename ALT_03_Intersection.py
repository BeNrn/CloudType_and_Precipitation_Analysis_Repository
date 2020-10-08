#script to intersect the cloud type and the precipitation information
#write both to a .csv table with contains:
# - ct values with respective "underlying" precipitation data
import numpy as np
import rasterio
import pandas as pd
import os
#import matplotlib.pyplot as plt
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/02_Data/"
ctDir = "CloudType_Preprocessing/"
rdDir = "Radolan_Preprocessing/"

outdir = "Intersection_CT_RD/"

ctList = os.listdir(workingDir+ctDir)
rdList = os.listdir(workingDir+rdDir)

#------------------------------------------------------------------------------
# 2 DATA INTERSECTION
#------------------------------------------------------------------------------
# #data intersection
fileEnding = "bbox.tif"

#filter unique time steps
timeList = []
for files_ct in ctList:
    #index = np.where(np.array(ctList)==files_ct)[0][0]
    timeList = timeList + [files_ct[3:15]]

#convert list to set removes double entries
#backconverting afterwards
timeList = list(set(timeList))

#actual data extraction
for timesteps in timeList:
    #extract the ct pixel values
    for filesCT in ctList:
        if filesCT.endswith(str(timesteps+"_CRS_"+fileEnding)):
            filenameCT = os.path.join(workingDir+ctDir, files_ct)
            ct = rasterio.open(filenameCT)
            ctData = ct.read(1)
            #iterate over the coordinates and retrieve lat lon
            #only one time needed
            if filesCT.endswith(str(timeList[0]+"_CRS_"+fileEnding)):
                #create storage DF
                latlonDF = pd.DataFrame(columns = np.arange(0,ct.width), index = np.arange(0,ct.height))
                for row in np.arange(0,ct.height):
                    for col in np.arange(0,ct.width):
                        latlonDF.iloc[row,col] = rasterio.transform.xy(transform = ct.transform, rows = row, cols = col)
            ct.close()
    
    #extract the radolan pixel values
    for filesRD in rdList:
        if filesRD.endswith(timesteps+"_CRS_Resample_"+fileEnding):
            filenameRD = os.path.join(workingDir+rdDir, filesRD)
            rad = rasterio.open(filenameRD)
            rdData = rad.read(1)
            rad.close()
            #set nodata values 
            rdData[rdData == -9999.] = np.nan
    #intersection
    isec = np.array([ctData,rdData])
    
    #--------------------------------------------------------------------------
    # 3 DATA QUERY
    #--------------------------------------------------------------------------
    #ct ID's 
    ct_vals = np.unique(isec[0,:,:])
    #all ct data have the same uniqeu ct vals (no 1, 2, 5)
    #print(ct_vals)

    #ct index with most rd responses has responses of 27.256 something
    rd_values_sorted = np.zeros((28000,6))
    rd_values_sorted[rd_values_sorted == 0.] = np.nan
    
    for value in ct_vals:   
        #rd data where ct equals cloud type value 
        rd_vals = isec[1,:,:][isec[0,:,:] == value]
        #last [0] is for getting the integer, not the array
        colSelection = np.where(ct_vals == value)[0][0]
        rowEnd = len(rd_vals)
        rd_values_sorted[0:rowEnd,colSelection] = rd_vals
        
    #--------------------------------------------------------------------------
    # 4 TRANSFORM DATA TO DATAFRAME AND WRITE TO HARDDRIVE
    #--------------------------------------------------------------------------
    #create an array with an additional column compared to rd_...
    rd_rows = rd_values_sorted.shape[0]
    rd_cols = rd_values_sorted.shape[1]
    restruct = np.zeros((rd_rows, rd_cols+1))
    restruct[:,1:] = rd_values_sorted
    #add an ID column
    restruct[:,0] = np.arange(1,28001)

    df = pd.DataFrame(restruct[:,1:], index = restruct[:,0])
    df.columns = ["CT_0", "CT_3", "CT_4", "CT_6", "CT_7", "CT_8"]

    df.to_csv(workingDir+outdir+"rd_values_"+timesteps+"_.csv",sep = ",")
        











# test = rd_values_sorted[:,0]
# test = test[~np.isnan(test)]
# plt.scatter(np.arange(len(test)),test)

#------------------------------------------------------------------------------
# 3 PLOTTING
#------------------------------------------------------------------------------
# fig, ((ax1, ax2), (ax3, ax4), (ax5, ax6)) = plt.subplots(3, 2)
# fig.tight_layout(pad=2.0)

# test = rd_values_sorted[:,0]
# test = test[~np.isnan(test)]

# ax1.scatter(np.arange(len(test)),test)
# ax1.set_title("CT = 0")

# test = rd_values_sorted[:,1]
# test = test[~np.isnan(test)]

# ax2.scatter(np.arange(len(test)),test)
# ax2.set_title("CT = 3")

# test = rd_values_sorted[:,2]
# test = test[~np.isnan(test)]

# ax3.scatter(np.arange(len(test)),test)
# ax3.set_title("CT = 4")

# test = rd_values_sorted[:,3]
# test = test[~np.isnan(test)]

# ax4.scatter(np.arange(len(test)),test)
# ax4.set_title("CT = 6")

# test = rd_values_sorted[:,4]
# test = test[~np.isnan(test)]

# ax5.scatter(np.arange(len(test)),test)
# ax5.set_title("CT = 7")

# test = rd_values_sorted[:,5]
# test = test[~np.isnan(test)]

# ax6.scatter(np.arange(len(test)),test)
# ax6.set_title("CT = 8")



#------------------------------------------------------------------------------
# ct_vals = np.unique(isec[0,:,:])

# nameList = ["rdWhereCt"]*len(ct_vals)

# test = np.where(ct_vals == 4)
# print(test[0])



# #ct Data
# isec[0,:,:]
# #ct Data where there is a 7
# isec[0,:,:] == 7 
# #rd data where ct has a 7
# rdWhereCt7 = isec[1,:,:][isec[0,:,:] == 7 ]
# rdWhereCt7 = rdWhereCt7[~np.isnan(rdWhereCt7)]

# plt.scatter(np.arange(len(rdWhereCt7)),rdWhereCt7)