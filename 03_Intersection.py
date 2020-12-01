#script to intersect the cloud type and the precipitation information
#write both to a .csv table with contains:
# - ct values with respective "underlying" precipitation data
#each day is saved in a single .csv
import numpy as np
import rasterio
import pandas as pd
import os
from glob import glob

#works only when the current working folder is "C:\Users\tamta\Documents\Studium\02_Master\17_Masterarbeit\CloudType_and_Precipitation_Analysis_Repository" 
import FUN_ExtractWeatherSituation as ws
#import matplotlib.pyplot as plt
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"
ctDir = "CloudType_Preprocessing/"
rdDir = "Radolan_Preprocessing/"
msgDir = "MSGSpectral_Preprocessing/"

outdir = "Intersection_CT_RD/"

ctList = os.listdir(workingDir + ctDir)
rdList = os.listdir(workingDir + rdDir)
msgList = os.listdir(workingDir + msgDir)

###############################################################################
#SPECIAL FOR PARTLY DATA INTERSECTION DEZEMBER 2017
#--------------------------------------------------
ctList = [element for element in ctList if element[7:9] == "12"]
rdList = [element for element in rdList if element[12:14] == "12"]
msgList = [element for element in msgList if element[8:10] == "12"]

#SPECIAL FOR PARTLY DATA INTERSECTION JULY 2017
#----------------------------------------------
# #remove all ct values from Dezember
# ctList = [element for element in ctList if element[7:9] != "12"]
# #remove the incomplete date 11.07. from the ct data
# ctList = [element for element in ctList if element[9:11] != "11"]

# #the same for radolan
# rdList = rdList[1:len(rdList)]
# rdList = [element for element in rdList if element[12:14] != "12"]
# #all dates that are not yet extracted: np.arange(11,32)
# for i in np.arange(11,32):
#     rdList = [element for element in rdList if element[14:16] != str(i)]   
###############################################################################
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
#Prerequisites:
#[X] adjust the month of the weather classification
#[X] define the output name and directory at the end of the loop
#--------
#loop over days
for days in dayList:
    #create empty dataframe for output storage
    df = pd.DataFrame(columns = ["acquisitionDate", "lat", "lon", "weather", "cloudType", "precipitation", "IR_039", "IR_087", "IR_097", "IR_108", "IR_120", "IR_134", "WV_062", "WV_073"])
    
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

    #actual data extraction
    for timesteps in timeList_day:
        #print(timesteps)
        #extract the ct pixel values
        for filesCT in ctList:
            if filesCT.endswith(str(timesteps+"_CRS_"+fileEnding)):
                filenameCT = os.path.join(workingDir+ctDir+filesCT)
                ct = rasterio.open(filenameCT)
                ctData = ct.read(1)
                 #--------------------------------------------------------------
                #iterate over the coordinates and retrieve lat lon
                #only one time needed
                if filesCT.endswith(str(timeList[0]+"_CRS_"+fileEnding)):
                    #create storage DF
                    latDF = pd.DataFrame(columns = [np.arange(0,ct.width)], index = np.arange(0,ct.height))
                    lonDF = pd.DataFrame(columns = [np.arange(0,ct.width)], index = np.arange(0,ct.height))
                    for row in np.arange(0,ct.height):
                        print(row)
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
                                       filename = "PrecipitationValues_DWD/Wetterlagenklassifikation.txt", 
                                       month = 12)
                                       #month = 7)
            wSit = wSit[int(days)-1]
            
        #extract the MSG channel data
      
        #filter for date
        #define an extra list for each timestep
        MSG_timestep_list = []
        for filesMSG in msgList:
            #slightly adapted filtering technique caused by MSG channel information in between
            if filesMSG.endswith("CRS_Resample_"+fileEnding) and filesMSG.startswith("MSG_"+timesteps):
                MSG_timestep_list = MSG_timestep_list + [filesMSG]
        
        #create a numpy array construction that can be filled
        msg_channelData = [[[0]*173]*233]*8
        msg_channelData = np.array(msg_channelData)
        allChannels = ["IR_039", "IR_087", "IR_097", "IR_108", "IR_120", "IR_134", "WV_062", "WV_073"]
        
        #iterate over created list
        for elements in MSG_timestep_list:
            #filter for channel
            for channels in allChannels:
                if elements[16:22] == channels:
                    filenameMSG = os.path.join(workingDir+msgDir, elements)
                    msg = rasterio.open(filenameMSG)
                    msg_channelData[list.index(allChannels, channels)] = msg.read(1)
                    msg.close()
                     
        #--------------------------------------------------------------------------
        # 3 DATA COLLECTION AND WRITING TO DISK
        #--------------------------------------------------------------------------
        #store ct and rad in df
        #acquisition date: to get the correct date for each scene (array with 233x173 pixel), 
        #  the date is converted from "201712010015" to "2017-12-01 00:15"
        data = {"acquisitionDate":dateList[timeList_day.index(timesteps)], 
                "lat":np.array(latDF, dtype = np.float).flatten(),
                "lon":np.array(lonDF, dtype = np.float).flatten(), "weather": wSit,
                "cloudType": ctData.flatten(), "precipitation": rdData.flatten(), 
                "IR_039": msg_channelData[0].flatten(), "IR_087": msg_channelData[1].flatten(), 
                "IR_097": msg_channelData[2].flatten(), "IR_108": msg_channelData[3].flatten(),
                "IR_120": msg_channelData[4].flatten(), "IR_134": msg_channelData[5].flatten(),
                "WV_062": msg_channelData[6].flatten(), "WV_073": msg_channelData[7].flatten()}
        df_temp = pd.DataFrame(data = data, index = np.arange(0,233*173))
        frame = [df, df_temp]
        df = pd.concat(frame)

    #write to disk
    df.to_csv(workingDir+outdir+"CT_RD_intersection_12" + days + ".csv",sep = ",")
    #df.to_csv(workingDir+outdir+"CT_RD_intersection_07" + days + "_TEST.csv",sep = ",")
    
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
