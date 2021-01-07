#this skript provides the evaluation of the results of the cluster analysis

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/ClusterAnalysis/")

library(FSA)
library(stringr)
library(magrittr)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
#table that contains the cluster values and its respective new value
lookUpTable <- read.csv(paste0(workingDir, "06_Auswertung/Clustergruppe-Zuordnung.csv"), sep = ";", dec = ",")
lookUpTable$Schluessel <- NULL
#remove empty columns
lookUpTable <- lookUpTable[lookUpTable$Datum != "",]

#list of day slices
list <- list.files(paste0(dataDir, "DaySlicing"))
list <- list[endsWith(list, ".csv")]
list <- list[str_sub(list, 9,9) == "5"]

for(j in 1:length(list)){
  df <- read.csv(paste0(dataDir, "DaySlicing/", list[j]))
  
  #-------------------------------------------------------------------------------
  #2 ASSIGN CONTINUING CLUSTER GROUP
  #-------------------------------------------------------------------------------
  #add a column and copy the cluster values
  df$ClusterGroup_continuing <- df$ClusterGroup
  #transform new column to string
  df$ClusterGroup_continuing <- as.character(df$ClusterGroup_continuing)
  
  #check for the row in the lookup table and assign the new naming
  for(i in 1:nrow(lookUpTable)){
    #create the same date format
    date <- paste0(str_sub(lookUpTable$Datum[i], 7,10), "-",
                   str_sub(lookUpTable$Datum[i], 4,5), "-",
                   str_sub(lookUpTable$Datum[i], 1,2), " ",
                   lookUpTable$Uhrzeit[i])
    #when found
    if(date == df$acquisitionDate[1]){
      #print(lookUpTable[i,3:7])
      #rename the groups
      df$ClusterGroup_continuing[df$ClusterGroup_continuing == "1"] <- lookUpTable[i,3]
      df$ClusterGroup_continuing[df$ClusterGroup_continuing == "2"] <- lookUpTable[i,4]
      df$ClusterGroup_continuing[df$ClusterGroup_continuing == "3"] <- lookUpTable[i,5]
      df$ClusterGroup_continuing[df$ClusterGroup_continuing == "4"] <- lookUpTable[i,6]
      df$ClusterGroup_continuing[df$ClusterGroup_continuing == "5"] <- lookUpTable[i,7]
      
      #test if groups are assigned correctly(order, content and no NAs )
      if(paste0(lookUpTable[i,3:7], collapse = "") == paste0(unique(df$ClusterGroup_continuing[df$ClusterGroup == 1]),
                                             unique(df$ClusterGroup_continuing[df$ClusterGroup == 2]),
                                             unique(df$ClusterGroup_continuing[df$ClusterGroup == 3]),
                                             unique(df$ClusterGroup_continuing[df$ClusterGroup == 4]),
                                             unique(df$ClusterGroup_continuing[df$ClusterGroup == 5]), collapse = "")){
        print("Correct group assignment.")
      }
      #save to hard drive
      daytime <- paste0(df$acquisitionDate[1] %>% str_sub(1,10),
                       "_",
                       df$acquisitionDate[1] %>% str_sub(12,13),
                       "_", 
                       df$acquisitionDate[1] %>% str_sub(15,16))
      write.csv(df, paste0(dataDir, "/DaySlicing_NewGroups/Cluster_newGroups_", daytime, ".csv"), row.names = F)
      }
    }
}

#-------------------------------------------------------------------------------
#3 EVALUATION OF THE RESULTS "REGRESSION"
#-------------------------------------------------------------------------------
#the cluster groups building on each other presented in the continuing time 
# series
#that allows an evaluation of each group beyond one single scene
#resulting in 5 groups (cluster 1 to 5) for July and 5 for December

#3.1 choose the month
#--------------------
month = "07"
#month = "12"

#3.2 load the cluster groups
#---------------------------
clusGroup <- c("red", "green", "blue", "yellow", "grey")

#3.3 load and prepare the data
#------------------------------
list <- list.files(paste0(dataDir, "DaySlicing_newGroups"))
list <- list[str_sub(list, 24,25) == month]

#if there is an old df in memory
rm(df_grps)
for(j in 1:length(clusGroup)){
  #empty precipitation data vector
  precip_clusGrp <- c()
  #extract the precipitation values for each cluster group
  for(i in 1:length(list)){
    df_temp <- read.csv(paste0(dataDir, "DaySlicing_newGroups/", list[i]))
    precipVals <- df_temp$precipitation[df_temp$ClusterGroup_continuing == clusGroup[j]]
    precip_clusGrp <- c(precip_clusGrp, precipVals)
  }
  #add the values together in a df in long format
  if(j == 1){
    df_grps <- data.frame(precip = rep(NA, length(precip_clusGrp)), group = rep(NA, length(precip_clusGrp)))
    df_grps$group <- "red"
    df_grps$precip <- precip_clusGrp
  }else{
    df_grps <- rbind(df_grps, data.frame(precip = precip_clusGrp, group = rep(clusGroup[j], precip_clusGrp %>% length())))
  }
}

df_grps$group <- as.factor(df_grps$group)

#3.4 Statistical evaluation
#--------------------------
#ANOVA VERFAHREN -> t-Test -> kann für diese Daten nicht angewendet werden
#The idea is to measure the proportion of the variance (of the variable) 
#explained by the group membership
# -> explained variance by the variable = between cluster variance + within cluster variance

#within cluster variance
# sum(var(df_grps$precip[df_grps$group == "red"]),
#     var(df_grps$precip[df_grps$group == "green"]),
#     var(df_grps$precip[df_grps$group == "blue"]),
#     var(df_grps$precip[df_grps$group == "yellow"]),
#     var(df_grps$precip[df_grps$group == "grey"]))
#between cluster variance


kruskal.test(precip ~ group, data = df_grps)

dunnResult <- FSA::dunnTest(precip ~ group, data = df_grps, method = "bh")
dunnResult

#JUL
# Comparison           Z       P.unadj         P.adj
# 1    blue - green   0.6015328  5.474852e-01  6.843565e-01
# 2     blue - grey  28.2493906 1.448089e-175 7.240446e-175
# 3    green - grey  26.7596119 9.544379e-158 3.181460e-157
# 4      blue - red   0.3933848  6.940353e-01  7.711504e-01
# 5     green - red  -0.2935336  7.691143e-01  7.691143e-01
# 6      grey - red -30.7060254 4.729441e-207 4.729441e-206
# 7   blue - yellow   5.2181852  1.806847e-07  3.011412e-07
# 8  green - yellow   4.3746169  1.216459e-05  1.737799e-05
# 9   grey - yellow -24.4795210 2.441145e-132 6.102862e-132
# 10   red - yellow   5.5025136  3.744144e-08  7.488288e-08

#DEZ
# Comparison          Z       P.unadj         P.adj
# 1    blue - green  24.949736 2.149515e-137 2.149515e-136
# 2     blue - grey  10.861360  1.761069e-27  3.522139e-27
# 3    green - grey -13.100029  3.291319e-39  8.228297e-39
# 4      blue - red   7.024731  2.144791e-12  3.063987e-12
# 5     green - red -14.924410  2.286479e-50  7.621595e-50
# 6      grey - red  -2.834580  4.588595e-03  4.588595e-03
# 7   blue - yellow   3.304727  9.506899e-04  1.056322e-03
# 8  green - yellow -23.785842 4.680229e-125 2.340114e-124
# 9   grey - yellow  -8.558776  1.140718e-17  1.901196e-17
# 10   red - yellow  -4.626044  3.727156e-06  4.658945e-06

write.csv(dunnResult$res, paste0(workingDir, "07_Ergebnisse/DunnResult_clusterAnalysis_perNewGroup_Dez.csv"), row.names = F)
#write.csv(dunnResult$res, paste0(workingDir, "07_Ergebnisse/DunnResult_clusterAnalysis_perNewGroup_Jul.csv"), row.names = F)

#other possibilities:
#https://stats.stackexchange.com/questions/18706/using-statistical-significance-test-to-validate-cluster-analysis-results
#- bootstrapping method?
#https://de.wikipedia.org/wiki/Bootstrapping-Verfahren

#https://en.wikipedia.org/wiki/Circular_analysis

#-------------------------------------------------------------------------------
#4 EVALUATION OF THE SPECTRAL GROUPS
#-------------------------------------------------------------------------------
#muss überarbeitet werden
#boxplot daten abgreifen erlaubt kein späteres erstellen von Boxplots
#stattdessen boxplots direkt aus daten generieren

#########################
#hier weiter
########################

#4.1 load the data like above
#----------------------------
month = "07"
#month = "12"

list <- list.files(paste0(dataDir, "DaySlicing_newGroups"))
list <- list[str_sub(list, 24,25) == month]

#4.2 prepare a df and predefinings
#---------------------------------
#predefinings 
clusGroup <- c("red", "green", "blue", "yellow", "grey")
channels <- c("IR_039", "IR_087", "IR_097", "IR_108", "IR_120", "IR_134", "WV_062", "WV_073")

#three layers are necessary:
# - month, regulated by datetime
# - clustergroup, regulated by clustergroup name
# - spectral channel, regulated by channel name
rm(df_grps, df_temp, precip_clusGrp, precipVals)

df_spec <- data.frame(datetime = NA, clusterGroup = NA, specChannel = NA, 
                      bxpl_lowWhisk = NA, bxpl_lowHin = NA, bxpl_median = NA, bxpl_higWhisk = NA, bxpl_higHin = NA)

#4.3 extract boxplot information 
#--------------------------------
#iterate over scenes
for(i in 1:length(list)){
  df_temp <- read.csv(paste0(dataDir, "DaySlicing_newGroups/", list[i]))
  
  #iterate over cluster elements
  for(j in clusGroup){
    df_clus_temp <- df_temp[df_temp$ClusterGroup_continuing == j,]
    
    #iterate over spectral channels
    for(k in channels){
      #information for df
      datetime <- df_temp[1,1]
      clusterGroup <- j
      specChannel <- k
      
      dat <- boxplot.stats(df_clus_temp[,names(df_clus_temp) == k])[[1]]
      if(is.na(df_spec[1,1])){
        df_spec <- data.frame(datetime = datetime, clusterGroup = clusterGroup, specChannel = specChannel, 
                              bxpl_lowWhisk = NA, bxpl_lowHin = NA, bxpl_median = NA, bxpl_higWhisk = NA, bxpl_higHin = NA)
        df_spec[,4:8] <- dat
      }else{
        datrow <- c(datetime, clusterGroup, specChannel, dat)
        df_spec <- rbind(df_spec, datrow)
      }
    }
  }
}

#write.csv(df_spec, paste0(dataDir, "/SpectralBoxplots_Dez.csv"), row.names = F)
write.csv(df_spec, paste0(dataDir, "/SpectralBoxplots_Jul.csv"), row.names = F)
