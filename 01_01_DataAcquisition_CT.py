#process the downloaded cloud type (CT) data
from satpy import Scene
import numpy as np
import os

#------------------------------------------------------------------------------
# 1 LOAD THE DATA
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/CloudType_NC/"
outputDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/CloudType_Raster/"

filelist = os.listdir(workingDir)
fileEnding = ".nc"

#------------------------------------------------------------------------------
# 2 CROP TO LOCAL EXTENT AND WRITE AS GEOTIFF
#------------------------------------------------------------------------------
for files in filelist:
    if files.endswith(fileEnding):
        filename = [os.path.join(workingDir, files)]
        #read the data
        #-------------
        #specify reader for the respective file type (here CM-SAF Claas 2 reader)
        #available from satpy 0.22 onwards, has to be installed using conda forge channel
        scnFile = {"cmsaf-claas2_l2_nc" : filename} 
        scn = Scene(filenames=scnFile)

        #show all available channels
        #scn.all_dataset_names()
        channel = scn.all_dataset_names()[3]

        #load the ct channel
        scn.load([channel])
        #scn[channel]
        #scn.show(channel)

        scn.available_composite_names()
        #crop the scene using the coordinates of the RADOLAN boundary (DWD 2004)
        #more in northern and southern direction because no perfect overlap after reprojection
        #crop function: ll_bbox=(xmin, ymin, xmax, ymax)
        #https://satpy.readthedocs.io/en/latest/api/satpy.html?highlight=crop#satpy.scene.Scene.crop
        scn_cropped = scn.crop(ll_bbox=(3.6, 46.7, 15.7, 55.2)) #y_original:ymin:47, ymax:54.7
        scn_cropped.show(channel)
        scn_cropped.save_datasets(writer = "geotiff",
                                  datasets = ["ct"],
                                  base_dir = outputDir, 
                                  filename = "CT_"+files[5:-17]+"_geotiff.tif",
                                  enhance = False,
                                  dtype = np.float32)