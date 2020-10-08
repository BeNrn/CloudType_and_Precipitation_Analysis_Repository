#script to intersect the cloud type and the precipitation information
#write both to a .csv table with contains:
# - ct values with respective "underlying" precipitation data
#each day is saved in a single .csv
import numpy as np
import rasterio
import pandas as pd
import os

#works only when the current working folder is "C:\Users\tamta\Documents\Studium\02_Master\17_Masterarbeit\04_Skripte" 
import FUN_ExtractWeatherSituation as ws
#import matplotlib.pyplot as plt
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"
ctDir = "CloudType_Preprocessing/"
rdDir = "Radolan_Preprocessing/"

outdir = "Intersection_CT_RD/"

ctList = os.listdir(workingDir+ctDir)
rdList = os.listdir(workingDir+rdDir)

#------------------------------------------------------------------------------
# 2 DATA INTERSECTION
#------------------------------------------------------------------------------
#2.1 Preliminary settings
#------------------------
fileEnding = "bbox.tif"

#filter unique time steps
timeList = []
for files_ct in ctList:
    #index = np.where(np.array(ctList)==files_ct)[0][0]
    timeList = timeList + [files_ct[3:15]]
    
#convert list to "set" removes double entries
#back-convertion afterwards
timeList = list(set(timeList))
timeList.sort()

#extract all available days
dayList = []
for days in timeList:
    dayList = dayList + [days[6:8]]
dayList = list(set(dayList))
dayList.sort()

#2.2 Loop over all single days
#-----------------------------
#loop over days
for days in dayList:
    #loop over timelist to extract all records from i'th day
    timeList_day = []
    for elements in timeList:
        if elements[6:8] == days:
            timeList_day = timeList_day + [elements]
            #results in a time list for day i
                
    #transform timeList_day into dateList for subsequent date conversion in R
    dateList = []
    for dates in timeList_day:
        dateList = dateList + [(dates[0:4] + "-" + dates[4:6] + "-" + dates[6:8] + " " + dates[8:10] + ":" + dates[10:12])]
   
    #create empty dataframe for output storage
    df = pd.DataFrame(columns = ["acquisitionDate", "lat", "lon", "weather", "cloudType", "precipitation"])

    #actual data extraction
    for timesteps in timeList_day:
        print(timesteps)
        #extract the ct pixel values
        for filesCT in ctList:
            if filesCT.endswith(str(timesteps+"_CRS_"+fileEnding)):
                filenameCT = os.path.join(workingDir+ctDir+filesCT)
                ct = rasterio.open(filenameCT)
                ctData = ct.read(1)
                #--------------------------------------------------------------
                #iterate over the coordinates and retrieve lat lon
                #only one time needed
                if filesCT.endswith(str(timeList_day[0]+"_CRS_"+fileEnding)):
                    #create storage DF
                    latDF = pd.DataFrame(columns = [np.arange(0,ct.width)], index = np.arange(0,ct.height))
                    lonDF = pd.DataFrame(columns = [np.arange(0,ct.width)], index = np.arange(0,ct.height))
                    for row in np.arange(0,ct.height):
                        for col in np.arange(0,ct.width):
                            latDF.iloc[row,col] = rasterio.transform.xy(transform = ct.transform, rows = row, cols = col)[1]
                            lonDF.iloc[row,col] = rasterio.transform.xy(transform = ct.transform, rows = row, cols = col)[0]
                #------------------------------------------------------------------
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
                
        #extract the weather situation
        #onle one time needed
        if timesteps == timeList_day[0]:
            wSit = ws.weatherSituation(workingDir = workingDir, 
                                       filename = "Wetterlagenklassifikation.txt", 
                                       month = 12)
            wSit = wSit[int(days)-1]
        
        #intersection
        isec = np.array([ctData,rdData])
        
        #--------------------------------------------------------------------------
        # 3 DATA COLLECTION AND WRITING TO DISK
        #--------------------------------------------------------------------------
        #store ct and rad in df
        #acquisition date: to get the correct date for each scene (array with 233x173 pixel), 
        #  the date is converted from "201712010015" to "2017-12-01 00:15"
        data = {"acquisitionDate":dateList[timeList_day.index(timesteps)], "lat":np.array(latDF, dtype = np.float).flatten(),
                "lon":np.array(lonDF, dtype = np.float).flatten(), "weather": wSit,
                "cloudType": ctData.flatten(), "precipitation": rdData.flatten()}
        df_temp = pd.DataFrame(data = data, index = np.arange(0,233*173))
        frame = [df, df_temp]
        df = pd.concat(frame)

    #write to disk
    df.to_csv(workingDir+outdir+"CT_RD_intersection_12" + days + ".csv",sep = ",")
    
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