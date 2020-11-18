workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "/03_Data/")

library(forcats)
library(ecodist)
library(magrittr)

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

set.seed(100)
smp <- sample(df$precipitation, 100)

#-------------------------------------------------------------------------------
#2 CLUSTER ANALYSIS - HIERARCHICAL CLUSTERING
#-------------------------------------------------------------------------------
#2.1 load the data
alter <- c(43, 38, 6, 47, 37, 9)

smp

#2.2 calculate the discance matrix
d <- dist(alter)

distMat <- dist(smp)

#2.3 hierarchical clustering
#method: distinguishes among others single-linkage, complete-linkage and average-linkage
e <- hclust(d, method = "complete", members = NULL)

cluster <- hclust(distMat, method = "complete", members = NULL)


#2.4 Quality of the clustering
#cophenetic distance matrix
coph <- cophenetic(e)
coph <- full(coph)

cophMat <- cophenetic(cluster)
cophMat <- full(cophMat)

#full distance matrix
dm <- ecodist::full(d)
distMat <- ecodist::full(distMat)

#correlation
cor(dm[lower.tri(dm)], coph[lower.tri(coph)])

cor(distMat[lower.tri(distMat)], cophMat[lower.tri(cophMat)])


#gamma coefficient
gammakoeffizient <- function(v1, v2){
  m1 <- outer(v1, v1, FUN = "<")
  m1 <- m1[lower.tri(m1)]
  m2 <- outer(v2, v2, FUN = "<")
  m2 <- m2[lower.tri(m2)]
  m3 <- outer(v1, v1, FUN = ">")
  m3 <- m3[lower.tri(m3)]
  m4 <- outer(v2, v2, FUN = ">")
  m4 <- m4[lower.tri(m4)]
  C <- sum((m1 + m2) == 2)
  C <- C + sum((m3 + m4) == 2)
  D <- sum((m1 + m4) == 2)
  D <- D + sum((m2 + m3) == 2)
  (C - D)/(C + D)
}

gammakoeffizient(dm[lower.tri(dm)], coph[lower.tri(coph)])
gammakoeffizient(distMat[lower.tri(distMat)], cophMat[lower.tri(cophMat)])

#Morjans Test
1+sum((e$height-mean(e$height))/sqrt(e$height)>1.25)
1+sum((cluster$height-mean(cluster$height))/sqrt(cluster$height)>1.25)


#Treppen plot
ch <- cluster$height

plot(rep(1,2),c(0,ch[1]),xaxt="n",yaxt="n",xlim=c(0,length(smp)),
     xaxs="i",yaxs="i",ylim=c(0,1),type="l",
     xlab="Anzahl Gruppen",ylab="Verschmelzungsniveau")
for(i in 2:length(ch)) lines(c(i,i),c(ch[i-1],ch[i]))
for (i in seq(1:(length(ch)-1))) lines(c(i,i+1),rep(ch[i],2))
#add axis labels
#xaxis
axis(1, at = 0:100,labels=100:0)
#yaxis
axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1))

#-------------------------------------------------------------------------------
#3 CLUSTER ANALYSIS - PARTITIONING  CLUSTERING
#-------------------------------------------------------------------------------
#load functions
source(paste0(workingDir, "CloudType_and_Precipitation_Analysis_Repository/FUN_Silhouetten.R"))

#load data
alter <- c(43,38,6,47,37,9)

#k-means function
#x........matrix of the data
#center...the center and the number of the classes, must be predefined 
#kmeans(x = matrix(alter,6,1), centers = 2)
e <- kmeans(x = matrix(alter,6,1), center = matrix(alter[1:2],2,1))
dm <- full(dist(alter))

#Mittelwerte der Klassen
e$centers

#welches Element in welcher Klasse
e$cluster

#innerhalb der Gruppen-Streuung
e$withinss

#größe der Klassen
e$size

#Berechnen der Silhouetten Werte mit der abgewandelten Silhouetten-Funktion
sil_params<- silhouette_params(cluster = e, distMat = dm, dat = alter)

#zeichnen der Silhouetten
plotsilho <- function(silinfo){
  #reversed s(i) values
  S <- rev(silinfo[[1]][, 3])
  #space between groups
  space <- c(0, rev(diff(silinfo[[1]][, 1])))
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
         center = matrix(alter[1:i],i,1)) %>% print()
  si[i-1]<-silhouette_params(cluster = e_g, distMat = dm, dat = alter)[[3]]
}
si

#Maßzahl G1 für 2 Klassen
k <- 2
spT <- sum((length(alter)-1)*var(alter))
spT
e <- kmeans(matrix(alter,ncol=1),matrix(alter[1:k],ncol=1))
spW <- sum(e$withinss)
spB <- spT-spW
(spB/spW)*(length(alter)-k)/(k-1)

#Für mehr als ein erhobenes Merkmal
n <- dim(PISA)[1]
spT <- sum(diag((n-1)*var(PISA)))
k <- 3
e <- kmeans(PISA,PISA[1:3,])
spW <- sum(e$withinss)
spB <- spT-spW
(spB/spW)*(n-k)/(k-1)
