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

#2.1 load the data
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

#Silhouette calculation function
source(paste0(workingDir, "CloudType_and_Precipitation_Analysis_Repository/FUN_Silhouetten.R"))

#3.2 Application solely with precipitation data
#----------------------------------------------
#no reasonable reasults, as the inter-group distance is larger than the in-between
#group discance (many elements at 0.1, few at 0.2 and 0.3, almost non with larger values)
#load data
smp

#k-means function
#x........matrix of the data
#center...the center and the number of the classes, must be predefined 
#kmeans(x = matrix(alter,6,1), centers = 2)
cluster <- kmeans(x = matrix(smp,length(smp),1), center = matrix(c(0.1, 0.3, 0.9),3,1))
dm <- full(dist(smp))

#Mittelwerte der Klassen
cluster$centers

#welches Element in welcher Klasse
cluster$cluster

#innerhalb der Gruppen-Streuung
cluster$withinss

#größe der Klassen
cluster$size

#Berechnen der Silhouetten Werte mit der abgewandelten Silhouetten-Funktion
sil_params<- silhouette_params(cluster = cluster, distMat = dm, dat = smp)

#zeichnen der Silhouetten
plotsilho <- function(silinfo){
  #reversed s(i) values
  S <- rev(silinfo[[1]][, 3])
  #space between groups
  space <- c(0, rev(diff(silinfo[[1]][,1])))
  space[space == -1] <- 1
  #names
  names <- rev(dimnames(silinfo[[1]])[[1]])
  if(!is.character(names))
    names <- as.character(names)
  barplot(S, space = space, names = names,
          xlab = "Breite der Silhouette", ylab = "",
          xlim = c(min(0, min(S)), 1), horiz = T,
          mgp = c(2.5, 1, 0))
  invisible()
}

plotsilho(sil_params)

#Silhouetten Werte in Abhängigkeit von der Klassenzahl
#Klassenzahl mit dem höchsten Wert entspricht der richtigen Klassenzahl
#Klassenzahl steigt auf wie folgt:
#1. 2Klassen
#2. 3Klassen ...
#si Werte, benötigte Anzahl gleich Variablenlänge -2 (da alle in einer Klasse und 
# ein Werte in jeweils einer Klasse nicht untersucht werden)
si <- rep(0,length(alter)-2)

#iteration über oben genannte Klassenzahlen
for(i in 2:(length(alter)-1)){
  e_g <- kmeans(x = matrix(alter,length(alter),1),
                center = matrix(alter[1:i],i,1))
  si[i-1]<-silhouette_params(cluster = e_g, distMat = dm, dat = alter)[[3]]
}
si

#3.3 Application with complete data
#-----------------------------------
#see: https://uc-r.github.io/kmeans_clustering
#load data
head(smp_totalDF)
smp_totalDF$lat <- NULL
smp_totalDF$lon <- NULL
smp_totalDF$cloudType <- NULL
smp_totalDF$acquisitionDate <- NULL
smp_totalDF$weather <- NULL

dm <- dist(smp_totalDF)
fviz_dist(dm, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#k-means function
#x........matrix of the data
#center...the center and the number of the classes, must be predefined 
#kmeans(x = matrix(alter,6,1), centers = 2)
cluster_2 <- kmeans(x = smp_totalDF, centers = 2)
cluster_3 <- kmeans(x = smp_totalDF, centers = 3)
cluster_4 <- kmeans(x = smp_totalDF, centers = 4)
cluster_5 <- kmeans(x = smp_totalDF, centers = 5)
cluster_6 <- kmeans(x = smp_totalDF, centers = 6)

p2 <- fviz_cluster(cluster_2, geom = "point", data = smp_totalDF) + ggtitle("k = 2")
p3 <- fviz_cluster(cluster_3, geom = "point", data = smp_totalDF) + ggtitle("k = 3")
p4 <- fviz_cluster(cluster_4, geom = "point", data = smp_totalDF) + ggtitle("k = 4")
p5 <- fviz_cluster(cluster_5, geom = "point", data = smp_totalDF) + ggtitle("k = 5")
p6 <- fviz_cluster(cluster_6, geom = "point", data = smp_totalDF) + ggtitle("k = 6")

grid.arrange(p2, p3, p4, p5, p6, nrow = 2, top = "Partitionierendes Clusterverfahren (kmeans), sample = 100\nUnterschiedliche Klassenzahl dargestellt auf 1. und 2. Hauptkomponente")

fviz_cluster(cluster, data = smp_totalDF)


#Mittelwerte der Klassen
cluster$centers

#welches Element in welcher Klasse
cluster$cluster

#innerhalb der Gruppen-Streuung
cluster$withinss

#größe der Klassen
cluster$size

set.seed(123)


fviz_nbclust(smp_totalDF, kmeans, method = "wss")

fviz_nbclust(smp_totalDF, kmeans, method = "silhouette")


# compute gap statistic
set.seed(123)
gap_stat <- clusGap(smp_totalDF, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Berechnen der Silhouetten Werte mit der abgewandelten Silhouetten-Funktion
sil_params<- silhouette_params(cluster = cluster, distMat = dm, dat = smp_totalDF)

#zeichnen der Silhouetten
plotsilho <- function(silinfo){
  #reversed s(i) values
  S <- rev(silinfo[[1]][, 3])
  #space between groups
  space <- c(0, rev(diff(silinfo[[1]][,1])))
  space[space == -1] <- 1
  #names
  names <- rev(dimnames(silinfo[[1]])[[1]])
  if(!is.character(names))
    names <- as.character(names)
  barplot(S, space = space, names = names,
          xlab = "Breite der Silhouette", ylab = "",
          xlim = c(min(0, min(S)), 1), horiz = T,
          mgp = c(2.5, 1, 0))
  invisible()
}

plotsilho(sil_params)

#Silhouetten Werte in Abhängigkeit von der Klassenzahl
#Klassenzahl mit dem höchsten Wert entspricht der richtigen Klassenzahl
#Klassenzahl steigt auf wie folgt:
#1. 2Klassen
#2. 3Klassen ...
#si Werte, benötigte Anzahl gleich Variablenlänge -2 (da alle in einer Klasse und 
# ein Werte in jeweils einer Klasse nicht untersucht werden)
si <- rep(0,length(alter)-2)

#iteration über oben genannte Klassenzahlen
for(i in 2:(length(alter)-1)){
  e_g <- kmeans(x = matrix(alter,length(alter),1),
                center = matrix(alter[1:i],i,1))
  si[i-1]<-silhouette_params(cluster = e_g, distMat = dm, dat = alter)[[3]]
}
si
