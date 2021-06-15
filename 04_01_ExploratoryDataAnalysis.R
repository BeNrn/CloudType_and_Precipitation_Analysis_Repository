#explorative data analysis, plots of the data

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"

library(forcats)
library(ggplot2)
library(ggridges)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
fileList <- list.files(paste0(workingDir, "Intersection_CT_RD/"))
#fileList <- fileList[1]

set.seed(1212)

for(files in fileList){
  print(files)
  df <- read.csv(paste0(workingDir, "Intersection_CT_RD/", files))[,-1]
  #round data to radolan accuracy of 1/10mm
  #df$precipitation <- round(df$precipitation, digits = 1)
  
  #remove zeros (zero precipitation is set to NA by python)
  df <- df[!is.na(df$precipitation),]
  #df <- df[df$precipitation != 0,]
  #adjust no-precipitation setting
  df <- df[df$precipitation > 0.01,]
  
  #remove cloudtypes that aren't interesting for precipitation study
  df <- df[!is.na(df$cloudType),] #NA
  df <- df[df$cloudType != 0,] #clear
  df <- df[df$cloudType != 7,] #cirrus
  
  #########################
  #only water group is examined
  #df <- df[df$cloudType == 3,]
  #########################
  
  #draw a sample of the valid data values
  #Moran's I test shows, that the null-hypothesis can be rejected for samples 
  # somewhere between 300(p = 0,0001) and 250 (p = 0,095) pixels
  #thus, valid sample number is found with 250 pixels 
  
  #when there are less than 250 entries take all of them 
  
  # if(nrow(df) > 10000){
  #   df <- df[sample(1:nrow(df), 10000),]
  # }
  
  if(nrow(df) > 250){
    df <- df[sample(1:nrow(df), 250),]
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
#2 DATA ON ITS OWN
#-------------------------------------------------------------------------------
#2.1 general plot
#-----------------
hist(df$precipitation)

plot(df$cloudType, df$precipitation,
     xlab = "Wolkenklassen",
     ylab = "Niederschlag in mm",
     main = paste0("Niederschlagswerte in den einzelnen Wolkenklassen\n(n = ", as.character(nrow(df)), ")\nDez. + Jul. - 10.000 Sample pro Tag"),
     log = "y",
     outline = F)

#2.2 Differences between the cloud types
#---------------------------------------
#boxplot, histogram and bxplot-value extraction for all cloud types

for(i in seq(1:length(unique(df$cloudType)))){
  #the cloud type name
  ct_name <- as.character(unique(df$cloudType)[i])
  title_name <- paste0("Cloud type ",ct_name, "\nn = ", as.character(length(df$precipitation[df$cloudType == ct_name])))
  #histogram
  hist(df$precipitation[df$cloudType == ct_name],
       xlim = c(0,2), 
       main = title_name,
       breaks = c(seq(0,14,0.1)),
       labels = T,
       col = "lightblue")
  #boxplot
  #boxplot(df$precipitation[df$cloudType == ct_name], 
  #        main = title_name,
  #        ylim = c(0,3))
}


#ridge line plot
ggplot(df, aes(x = precipitation, y = cloudType, fill = cloudType))+
  geom_density_ridges(stat="binline", alpha=0.6, bins=16)+
  theme_ridges()+ 
  xlim(0, 1.5)+
  theme(legend.position = "none")

#-------------------------------------------------------------------------------
#3 DATA WITH THE WEATHER SITUATION
#-------------------------------------------------------------------------------
#3.1 Every cloud type on different weather situation
#----------------------------------------------------
for(i in seq(1:length(unique(df$cloudType)))){
  #the cloud type name
  ct_name <- as.character(unique(df$cloudType)[i])
  title_name <- paste0("Cloud type ",ct_name, "\nn = ", as.character(length(df$precipitation[df$cloudType == ct_name])))
  #boxplot with weather type
  ct_val <- df[df$cloudType == ct_name,]
  plot(as.factor(ct_val$weather), 
       ct_val$precipitation, 
       ylim = c(0,3), 
       main = title_name,
       xlab = "Weather situation",
       ylab = "Precipitation in mm")
  #data values
  print(ct_name)
  print("----------------")
  print(unique(df$weather))
  for(j in seq(1:length(unique(df$weather)))){
    print(summary(ct_val$precipitation[ct_val$weather == unique(df$weather)[j]])) 
  }
}

#3.2 Every weather situation on different cloud types
#----------------------------------------------------
op <- par()
par(mfrow = c(2,2))
for(i in seq(1:length(unique(df$weather)))){
  weatherNumber <- unique(df$weather)[i]
  plot(df$cloudType[df$weather == weatherNumber], df$precipitation[df$weather == weatherNumber],
       xlab = "Wolkenklassen",
       ylab = "Niederschlag in mm",
       main = paste0("Niederschlag je Wolkenklassen nach Wetterlage (n = ", as.character(nrow(df)), ")\nDez17 komplett - 10.000 Sample pro Tag\nWetterlage",weatherNumber),
       log = "y",
       outline = F)
}
par(op)

#mean
mean(df$precipitation[df$cloudType == "water"])

