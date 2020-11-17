#process the MSG spectral information

import rioxarray
import xarray
import os
#------------------------------------------------------------------------------
# 1 LOAD THE DATA
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/MSGSpectral_NC/"
outputDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/MSGSpectral_Raster/"

filelist = os.listdir(workingDir)
fileEnding = ".nc"

#------------------------------------------------------------------------------
# 2 CROP TO LOCAL EXTENT AND WRITE AS GEOTIFF
#------------------------------------------------------------------------------
for files in filelist:
    if files.endswith(fileEnding):
        filename = os.path.join(workingDir, files)
        #read the data as xarray
        #-----------------------
        dat = xarray.open_dataset(filename)  
        #show all available channels
        #proj4 must be ignored
        #dat.data_vars
        
        #chose each channel one by one
        #-----------------
        #all needed channels are:
            #IR_039
            #IR_087
            #IR_097
            #IR_108
            #IR_120
            #IR_134
            #WV_062
            #WV_073
        allChannels = ("IR_039", "IR_087", "IR_097", "IR_108", "IR_120", "IR_134", "WV_062", "WV_073")
        
        for channels in allChannels:
            band = channels
            #show the channel
            #dat[band].plot()
            
            #define the xarray (contains information of that one channel only)
            dat_band = dat[band]
         
            #adjust CRS
            dat_band = dat_band.rio.set_crs("EPSG:4326")
     
            min_lon = 3.6
            min_lat = 46.7
            max_lon = 15.7
            max_lat = 55.2
        
            subset = dat_band.rio.clip_box(minx=min_lon, miny=min_lat, maxx=max_lon, maxy=max_lat)
        
            #show
            #subset.plot()
        
            #save as .tif
            subset.rio.to_raster(outputDir + "MSG_201707011300_" + channels + ".tif")
