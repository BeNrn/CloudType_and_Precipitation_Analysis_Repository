#process the downloaded Radolan  data
import os
import tarfile
import wradlib as wrl
import warnings
warnings.filterwarnings('ignore')
#------------------------------------------------------------------------------
# 1 UNZIP THE DATA
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/Radolan_bin/"

monthFolder = os.listdir(workingDir+"monthFolder")
monthDir = workingDir+"monthFolder/"+monthFolder[0]
dayFolder = os.listdir(monthDir)
dayFolder = dayFolder[1:]

#get the day files
for month in dayFolder:
    #create input path (one per day)
    fullPath = monthDir+"/"+ month
    #create output path
    fullPath_out = workingDir+"binData"
    #extract the data
    tar = tarfile.open(fullPath)
    tar.extractall(fullPath_out)
    tar.close()

#------------------------------------------------------------------------------
# 2 SET FOLDER PATHS
#------------------------------------------------------------------------------
outdir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/Radolan_Raster"
filelist = os.listdir(fullPath_out)
fileEnding = "bin"

#------------------------------------------------------------------------------
# 3 WRITE AS GEOTIFF
#------------------------------------------------------------------------------
for files in filelist:
    if files.endswith(fileEnding):
        filename = os.path.join(fullPath_out, files)
        #read the data
        data_raw, meta = wrl.io.read_radolan_composite(filename)

        #get the projection coordinates from the Radolan grid
        # This is the RADOLAN projection
        proj_osr = wrl.georef.create_osr("dwd-radolan")
        # Get projected RADOLAN coordinates for corner definition
        xy_raw = wrl.georef.get_radolan_grid(1100, 900)

        #check origin and row/column order
        #coordinates from lower left corner to upper right one
        data, xy = wrl.georef.set_raster_origin(data_raw, xy_raw, 'upper')

        #create the geotiff
        ds = wrl.georef.create_raster_dataset(data, xy, projection=proj_osr)
        wrl.io.write_raster_dataset(outdir + "/Radolan_"+files[8:12]+files[25:33]+"_geotiff.tif", ds, 'GTiff')