#- prior regression
#- selection of a weather situation -> 39
#- weather situation probably depicts rain events in a good resolution 
#- cut days with this weather situation in its single scenes
#- cluster analysis should be performed on single scenes and the propagation of the 
#identified cloud clusters should be monitored
#a) the cloud clusters are building on each other -> the scenes could be analyzed together
#b) they do not build on each other -> each scene should be analyzed by its own
#ws...weather situation

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"

library(magrittr)
library(stringr)
library(factoextra)
library(rgdal)
library(raster)
library(tidyverse)
library(cluster)
#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
#ws = 39
#July: on the 12.07.
#December: on the 30.12.

#load one month 
#month = "07"
month = "12"

fileList <- list.files(paste0(workingDir, "Intersection_CT_RD/"))
if(month == "07"){
  df_file <- fileList[str_sub(fileList, -8,-5) == "0712"]
}else if(month == "12"){
  df_file <- fileList[str_sub(fileList, -8,-5) == "1230"]
}
#load the df at the identified position
df <- read.csv(paste0(workingDir, "Intersection_CT_RD/", df_file))[,-1]

#round data to radolan accuracy of 1/10mm
#df$precipitation <- round(df$precipitation, digits = 1)
#remove zeros (zero precipitation is set to NA by python)
df <- df[!is.na(df$precipitation),]
#df <- df[df$precipitation != 0,]
df <- df[df$precipitation > 0.01,]

#remove cloudtypes that aren't interesting for precipitation study
df <- df[!is.na(df$cloudType),] #NA
df <- df[df$cloudType != 0,] #clear
df <- df[df$cloudType != 7,] #cirrus

#-------------------------------------------------------------------------------
#2 SLICING
#-------------------------------------------------------------------------------
#split the df into 96 single elements (the single time steps)
df_list <- list()
for(i in 1:(df$acquisitionDate %>% unique() %>% length())){
  df_list[[i]] <- df[df$acquisitionDate == (df$acquisitionDate %>% unique())[i],]
}

#-------------------------------------------------------------------------------
#3 CLUSTER ANALYSIS AND WRITING TO DISK
#-------------------------------------------------------------------------------
for(i in 1:length(df_list)){
  smp <- df_list[[i]]
  #remove columns (all with a variance of zero (only one characteristic))
  smp$lat <- NULL
  smp$lon <- NULL
  smp$cloudType <- NULL
  smp$acquisitionDate <- NULL
  smp$weather <- NULL
  smp$precipitation <- NULL
    
  #test for the optimal class number statistically
  set.seed(123)
  #4.6.1 ellbow method
  #factoextra::fviz_nbclust(smp, kmeans, method = "wss")
  #3,4,5

  #4.6.2 silhouette method
  #factoextra::fviz_nbclust(smp, kmeans, method = "silhouette")
  #2, 4

  #4.6.3 gap method
  #gap_stat <- clusGap(smp, FUN = kmeans, nstart = 25,
  #                    K.max = 10, B = 50)
  #print(gap_stat, method = "firstmax")
  #factoextra::fviz_gap_stat(gap_stat)
  #4,5,7
  
  #final cluster analysis
  cluster <- kmeans(smp, center = 5, nstart = 25)
  #cluster <- kmeans(smp, center = 4, nstart = 25)
  #save to disk
  
  #smp %>% mutate(ClusterGroup = cluster$cluster) %>% group_by(ClusterGroup) %>% summarise_all("mean")
  
  df_out <- df_list[[i]] %>% mutate(ClusterGroup = cluster$cluster)
  
  timeStep <- paste0(df_out$acquisitionDate[1] %>% str_sub(1,10),
                     "_", 
                     df_out$acquisitionDate[1] %>% str_sub(12,13),
                     "_", 
                     df_out$acquisitionDate[1] %>% str_sub(15,16))
  
  write.csv(df_out, paste0(workingDir, "ClusterAnalysis/DaySlicing/Cluster_5_", timeStep, ".csv"), row.names = F)
  print(i)
}

#-------------------------------------------------------------------------------
#4 LOAD DATA AGAIN AND WRITING TO TIFF
#-------------------------------------------------------------------------------
#only for chosen month and only 5-group-cluster
list <- list.files(paste0(workingDir, "ClusterAnalysis/DaySlicing"))
list <- list[str_sub(list,11,20) == "2017-12-30"]
#list <- list[str_sub(list,11,20) == "2017-07-12"]

list <- list[str_sub(list, 9,9) == "5"]

sp::spTransform()

#iterate
for(listelement in list){
  print(listelement)
  #read data
  df <- read.csv(paste0(workingDir, "ClusterAnalysis/DaySlicing/",as.character(listelement)))
  #df to spatial points data frame
  coordinates(df) <- ~ lon + lat
  #spdf to spatial pixels data frame
  gridded(df) <- TRUE
  #to raster
  df_rast <- stack(df)
  projection(df_rast) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" 
  #save
  writeRaster(df_rast, paste0(workingDir, "ClusterAnalysis/DaySlicing_Tiff/", listelement %>% str_sub(1,-5), ".tif"), format = "GTiff", overwrite = TRUE)
}

#-------------------------------------------------------------------------------
#5 SHOW THE RESULTS
#-------------------------------------------------------------------------------
# list <- list.files(paste0(workingDir, "ClusterAnalysis/DaySlicing_Tiff"))
# list <- list[str_sub(list, -3,-1) == "tif"]
# 
# for(listelement in list){
#   
#   scene <- raster(paste0(workingDir, "ClusterAnalysis/DaySlicing_Tiff/", listelement), band = 13)
#   png(filename = paste0(workingDir, "ClusterAnalysis/DaySlicing_Img/", listelement %>% str_sub(1,-5), ".png"))
#   plot(scene, col = rainbow(4))
#   dev.off()
# }
