workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "/03_Data/")

library(forcats)
library(ecodist)
library(magrittr)
library(tidyverse)
library(factoextra)
library(cluster)
library(gridExtra)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
fileList <- list.files(paste0(dataDir, "Intersection_CT_RD/"))
fileList <- fileList[2]

for(files in fileList){
  print(files)
  df <- read.csv(paste0(dataDir, "Intersection_CT_RD/", files))[,-1]
  #round data to radolan accuracy of 1/10mm
  df$precipitation <- round(df$precipitation, digits = 1)
  #remove zeros (zero precipitation is set to NA by python)
  df <- df[!is.na(df$precipitation),]
  df <- df[df$precipitation != 0,]
  
  #remove cloudtypes that aren't interesting for precipitation study
  df <- df[!is.na(df$cloudType),] #NA
  df <- df[df$cloudType != 0,] #clear
  df <- df[df$cloudType != 7,] #cirrus
  
  #draw a sample of the valid data values
  #when there are less than 10000 entries take all of them 
  set.seed(121212)
  if(nrow(df) > 10000){
    df <- df[sample(1:nrow(df), 10000),]
  }
  
  df$cloudType <- as.factor(df$cloudType)
  #rename the cloud types
  df <- dplyr::mutate(df, cloudType = fct_recode(df$cloudType, "water" = "3", "supercooled" = "4", "mixed" = "5", "opaque_ice" = "6", "overlap" = "8", "overshooting" = "9"))
  
  #transform the first column into a time-date format
  df$acquisitionDate <- as.POSIXct(df$acquisitionDate, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  #print(head(df))
  if(nrow(df[is.na(df$precipitation),]) > 0){
    print(df[is.na(df$precipitation),])
  }else{print("Everything is awesome.")}
  #4, 7, 14, 31
  
  #merge the df's
  if(files == fileList[1]){
    df_total <- df
  }else{
    df_total <- rbind(df_total, df)
  }
}
df <- df_total
rm(df_total)

#draw a sample
set.seed(100)
smp <- sample(df$precipitation, 100)

smp_totalDF <- df[sample(1:nrow(df), 100),]

#-------------------------------------------------------------------------------
#2 CLUSTER ANALYSIS - HIERARCHICAL CLUSTERING
#-------------------------------------------------------------------------------
#see:
# - https://uc-r.github.io/hc_clustering
# - Handl, A., & Kuhlenkasper, T. (2017). Multivariate Analysemethoden. https://doi.org/10.1007/978-3-662-54754-0 (Kapitel 13 - Clusteranalyse)

#gamma coefficient function
source(paste0(workingDir, "CloudType_and_Precipitation_Analysis_Repository/FUN_Gammacoefficient.R"))

#2.1 prepare the data
#remove all variables that only have one characteristic 
smp_totalDF$acquisitionDate <- NULL
smp_totalDF$weather <- NULL
#remove all variables that should not be considered in the cluster analysis  
smp_totalDF$cloudType <- NULL
smp_totalDF$lat <- NULL
smp_totalDF$lon <- NULL

#2.2 calculate the discance matrix
distMat <- dist(smp_totalDF)
#2.3 hierarchical clustering
cluster <- hclust(distMat, method = "complete", members = NULL)

#2.4 plot
plot(cluster)

#2.5 divide the data in k groups by its height in the dendrogram
plot(cluster)
rect.hclust(cluster, k = 6, border = 2:6)

#2.6 divide data in subgroups and add this information to the df
sub_grp <- cutree(cluster, k = 6)
table(sub_grp)
#smp_totalDF %>% mutate(cluster = sub_grp) %>% head

#2.7 show the clustering of the data, using the first two principle components 
#Die ersten beiden Hauptkomponenten drücken Achsen im n-dimensionalen Merkmalsraum 
#mit der höchsten Variabilität aus -> lassen sich keinen Variablen mehr zuordnen,
#aber sind die beste Representation der Datenverteilung
fviz_cluster(list(data = smp_totalDF, cluster = sub_grp))

#2.8 analyze the best number of groups by comparing the clustering plots
sub_grp <- cutree(cluster, k = 2)
p2<- fviz_cluster(list(data = smp_totalDF, cluster = sub_grp)) + ggtitle("k = 2")
sub_grp <- cutree(cluster, k = 3)
p3<- fviz_cluster(list(data = smp_totalDF, cluster = sub_grp)) + ggtitle("k = 3")
sub_grp <- cutree(cluster, k = 4)
p4<-fviz_cluster(list(data = smp_totalDF, cluster = sub_grp)) + ggtitle("k = 4")
sub_grp <- cutree(cluster, k = 5)
p5<-fviz_cluster(list(data = smp_totalDF, cluster = sub_grp)) + ggtitle("k = 5")
sub_grp <- cutree(cluster, k = 6)
p6<-fviz_cluster(list(data = smp_totalDF, cluster = sub_grp)) + ggtitle("k = 6")

gridExtra::grid.arrange(p2, p3, p4, p5, p6, nrow = 2, top = "Hierarchisches Clusterverfahren, sample = 100\nUnterschiedliche Klassenzahl dargestellt auf 1. und 2. Hauptkomponente")

#2.9 analyze the optimal number of groups using statistical methods
#2.9.1 Ellbow method
fviz_nbclust(smp_totalDF, FUN = hcut, method = "wss")
#2.9.2 Silhouette method
fviz_nbclust(smp_totalDF, FUN = hcut, method = "silhouette")
#2.9.3 Gap method
gap_stat <- clusGap(smp_totalDF, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#-------------------------------------------------------------------------------
#3 CLUSTER ANALYSIS - PARTITIONING  CLUSTERING
#-------------------------------------------------------------------------------
#see:
# - https://uc-r.github.io/kmeans_clustering
# - Handl, A., & Kuhlenkasper, T. (2017). Multivariate Analysemethoden. https://doi.org/10.1007/978-3-662-54754-0 (Kapitel 13 - Clusteranalyse)

#3.1 prepare  the data
head(smp_totalDF)
smp_totalDF$lat <- NULL
smp_totalDF$lon <- NULL
smp_totalDF$cloudType <- NULL
smp_totalDF$acquisitionDate <- NULL
smp_totalDF$weather <- NULL

#3.2 distance matrix and general function call
#distance matrix
dm <- dist(smp_totalDF)
#cluster analysis
cluster <- kmeans(smp_totalDF, center = 4)

#3.3 visualize the similarity of the data
#as more different the data values as higher the value
fviz_dist(dm, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#3.4 compare cluster results for different class numbers
cluster_2 <- kmeans(x = smp_totalDF, centers = 2)
cluster_3 <- kmeans(x = smp_totalDF, centers = 3)
cluster_4 <- kmeans(x = smp_totalDF, centers = 4, nstart = 25)
cluster_5 <- kmeans(x = smp_totalDF, centers = 5)
cluster_6 <- kmeans(x = smp_totalDF, centers = 6)

p2 <- fviz_cluster(cluster_2, geom = "point", data = smp_totalDF) + ggtitle("k = 2")
p3 <- fviz_cluster(cluster_3, geom = "point", data = smp_totalDF) + ggtitle("k = 3")
p4 <- fviz_cluster(cluster_4, geom = "point", data = smp_totalDF) + ggtitle("k = 4")
p5 <- fviz_cluster(cluster_5, geom = "point", data = smp_totalDF) + ggtitle("k = 5")
p6 <- fviz_cluster(cluster_6, geom = "point", data = smp_totalDF) + ggtitle("k = 6")

grid.arrange(p2, p3, p4, p5, p6, nrow = 2, top = "Partitionierendes Clusterverfahren (kmeans), sample = 100\nUnterschiedliche Klassenzahl dargestellt auf 1. und 2. Hauptkomponente")

#3.5 stats
#mean of the classes
cluster$centers
#which element in which calss
cluster$cluster
#within-group scattering
cluster$withinss
#size of the classes
cluster$size

#3.6 test for the optimal class number statistically
set.seed(123)
#3.6.1 ellbow method
fviz_nbclust(smp_totalDF, kmeans, method = "wss")
#3.6.2 silhouette method
fviz_nbclust(smp_totalDF, kmeans, method = "silhouette")
#3.6.3 gap method
gap_stat <- clusGap(smp_totalDF, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#3.7 save findings in df
#3.7.1 identify the optimal group number using the methods in 3.6 
#and the visual appreaence in 3.4
#3.7.2 perform (if not already done) the cluster analysis
cluster <- kmeans(smp_totalDF, center = 4, nstart = 25)
#3.7.3 save the group number in the data
#printout of structure
smp_totalDF %>% mutate(ClusterGroup = cluster$cluster) %>% group_by(ClusterGroup) %>% summarise_all("mean")
df_out <- df %>% mutate(ClusterGroup = cluster$cluster)

write.csv(df_out, paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr_kmeans.csv"), row.names = F)
