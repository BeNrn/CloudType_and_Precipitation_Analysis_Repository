file_base <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"

library(forcats)
library(ecodist)
# library(caret)
# library(mgcv)
# library(ape)
# library(FSA)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
fileList <- list.files(paste0(file_base, "Intersection_CT_RD/"))
fileList <- fileList[1]

for(files in fileList){
  print(files)
  df <- read.csv(paste0(file_base, "Intersection_CT_RD/", files))[,-1]
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

#-------------------------------------------------------------------------------
#2 CLUSTER ANALYSIS -HIERARCHICAL CLUSTERING
#-------------------------------------------------------------------------------
#2.1 load the data
alter <- c(43, 38, 6, 47, 37, 9)

#2.2 calculate the discance matrix
d <- dist(alter)

#2.3 hierarchical clustering
#method: distinguishes among others single-linkage, complete-linkage and average-linkage
e <- hclust(d, method = "complete", members = NULL)

#zeigt die Verschmolzenen Objekte je Stufe
#Zahlen mit Minus zeigen das Verschmelzen von Objekten, positive Zahlen das 
#Verschmelzen von Gruppen (enthalten mehrere Objekte) und in welcher Stufe diese
#verschmolzen wurden
e$merge

#Abstand bei welchem die Verschmelzungen zusatande gekommen sind die in e$merge
#gezeigt werden
e$height

#Reihenfolge, in der die Objekte gezeichnet werden müssten damit sich das resul-
#tierende Dendrogramm nicht überschneiden würde
e$order

#plot a dendrogram
par0 <- par()
par(las=1)
plot(e)
par(par0)

#2.4 quality of the results
#generating a cophenetic distance matrix
coph <- cophenetic(e)

#create  full distance matrices for distance matrix and cophenetic distance matrix
dm <- ecodist::full(d)
coph <- full(coph)

#correlation
#use the values in the lower triangle of the matrices
cor(dm[lower.tri(dm)], coph[lower.tri(coph)])

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

#2.5 class number (Mojena test)
#Verschmelzungsniveaus
e$height

#Standardisieren der Werte und abgleich mit 1.25 Schwelle
1+sum((e$height-mean(e$height))/sqrt(e$height)>1.25)

#Treppen plot
eh <- e$height
plot(rep(1,2),c(0,eh[1]),xaxt="n",yaxt="n",xlim=c(0,7),
     xaxs="i",yaxs="i",ylim=c(0,50),type="l",
     xlab="Anzahl Gruppen",ylab="Verschmelzungsniveau")
for(i in 2:5) lines(c(i,i),c(eh[i-1],eh[i]))
for (i in 1:4) lines(c(i,i+1),rep(eh[i],2))
axis(1,at=0:5,labels=6:1)

#zuordnung der Elemente zu den Clustern
welche.cluster <- function(anz, hierar){
  co <- full(cophenetic(hierar))
  h <- hierar$height[length(hierar$height) + 1 - anz]
  n <- ncol(co)
  cl <- rep(0, n)
  k <- 1
  for(i in 1:n) {
    if(cl[i] == 0) {
      ind <- (1:n)[co[i, ] <= h]
      cl[ind] <- k
      k <- k + 1
    }
  }
  cl
}

welche.cluster(hierar = e, anz = 2)
