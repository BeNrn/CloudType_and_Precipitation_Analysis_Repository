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


#-------------------------------------------------------------------------------
#3 CT-PRECIP MEAN VALUE ANALYSIS
#-------------------------------------------------------------------------------
df <- data.frame(group = c("Supercooled", "Water", "Overlap", "Opaque_ice", "Overshooting"), meanVal = c(0.08534173, 0.1151635, 0.1466083, 0.1493126, 0.3674196))

#avoid sorting by name by transforming the text into sorted factors
df$group<- factor(df$group, levels = df$group)

#plot
ggplot(data = df, aes(x=group, y=meanVal)) +
  geom_bar(stat="identity", fill = "#0044a3")
