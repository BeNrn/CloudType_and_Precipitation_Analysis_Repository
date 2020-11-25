workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "/03_Data/")

library(raster)
library(rgdal)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
df <- read.csv(paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr_kmeans.csv"))
#df to spatial points data frame
coordinates(df) <- ~ lon + lat

#spdf to spatial pixels data frame
gridded(df) <- TRUE

df_rast <- stack(df)
projection(df_rast) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" 

writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr.tif"), format = "GTiff", overwrite = TRUE)
