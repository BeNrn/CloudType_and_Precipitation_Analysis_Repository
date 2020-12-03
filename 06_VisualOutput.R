workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "/03_Data/")

library(raster)
library(rgdal)
library(magrittr)
library(stringr)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
list <- list.files(paste0(dataDir, "ClusterAnalysis"))
list <- list[endsWith(list, ".csv")]
list <- list[8:length(list)]

#iterate
for(listelement in list){
  #read data
  df <- read.csv(paste0(dataDir, "ClusterAnalysis/",as.character(listelement)))
  #df to spatial points data frame
  coordinates(df) <- ~ lon + lat
  #spdf to spatial pixels data frame
  gridded(df) <- TRUE
  #to raster
  df_rast <- stack(df)
  projection(df_rast) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" 
  #save
  #writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr_4klassen", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
  writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/12_12_2017_02Uhr_", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
}
