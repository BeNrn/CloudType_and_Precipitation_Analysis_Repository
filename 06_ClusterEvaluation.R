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
      write.csv(df, paste0(dataDir, "/DaySlicing_NewGroups/Cluster_newGroups_", daytime, ".csv"))
      }
    }
}


#-------------------------------------------------------------------------------
#3 EVALUATION OF THE RESULTS
#-------------------------------------------------------------------------------
#the cluster groups building on each other as could be seen in the continuing
# time series
#that allows an evaluation of each group beyond one single scene
#resulting in 5 groups (cluster 1 to 5) for July and 5 for December

#3.1 choose the month
#--------------------
#month = "07"
month = "12"

#3.2 choose the cluster group
#-----------------------------
clusGroup <- c("red", "green", "blue", "yellow", "grey")
#clusGroup <- "red"
#clusGroup <- "green"
#clusGroup <- "blue"
#clusGroup <- "yellow"
#clusGroup <- "grey"

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
write.csv(dunnResult$res, paste0(workingDir, "07_Ergebnisse/DunnResult_clusterAnalysis_perNewGroup_Dez.csv"), row.names = F)
#write.csv(dunnResult$res, paste0(workingDir, "07_Ergebnisse/DunnResult_clusterAnalysis_perNewGroup_Jul.csv"), row.names = F)

#other possibilities:
#https://stats.stackexchange.com/questions/18706/using-statistical-significance-test-to-validate-cluster-analysis-results
#- bootstrapping method?
#https://de.wikipedia.org/wiki/Bootstrapping-Verfahren

#https://en.wikipedia.org/wiki/Circular_analysis
