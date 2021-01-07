#skript of visual output production

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/")

library(raster)
library(rgdal)
library(magrittr)
library(stringr)
library(ggplot2)

#-------------------------------------------------------------------------------
#1 ClusterAnalysis - WRITING AS TIFF
#-------------------------------------------------------------------------------
list <- list.files(paste0(dataDir, "ClusterAnalysis/DaySlicing/"))
list <- list[endsWith(list, ".csv")]
#list <- c(list[1:2], list[4:5], list[11:14])
#list <- list[7:10]

#iterate
for(listelement in list){
  #read data
  df <- read.csv(paste0(dataDir, "ClusterAnalysis/DaySlicing/",as.character(listelement)))
  #df to spatial points data frame
  coordinates(df) <- ~ lon + lat
  #spdf to spatial pixels data frame
  gridded(df) <- TRUE
  #to raster
  df_rast <- stack(df)
  projection(df_rast) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" 
  #save
  #writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr_4klassen", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
  #writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/12_12_2017_02Uhr_", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
  writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/DaySlicing_Tiff/", listelement %>% str_sub(1,16),"_", listelement %>% str_sub(25,33), ".tif"), format = "GTiff", overwrite = TRUE)
}

#-------------------------------------------------------------------------------
#2 ClusterAnalysis - BOXPLOT ANALYSIS
#-------------------------------------------------------------------------------
df <- read.csv(paste0(dataDir, "ClusterAnalysis/SpectralBoxplots_Jul.csv"))
df_sceneSlice <- df[df$datetime == "2017-07-12 00:00",]
df_colSlice <- df_sceneSlice[df_sceneSlice$clusterGroup == "red",]

ggplot(data = df_colSlice)+
  geom_point(mapping = aes(x = specChannel, y = bxpl_median), col = "red")

             