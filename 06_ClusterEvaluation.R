#this skript provides the evaluation of the results of the cluster analysis

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/ClusterAnalysis/")

library(FSA)
library(stringr)
library(magrittr)
library(forcats)
library(stringr)
library(ggplot2)

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
#3 EVALUATION OF THE MEAN VALUES
#-------------------------------------------------------------------------------
#the cluster groups building on each other presented in the continuing time 
# series
#that allows an evaluation of each group beyond one single scene
#resulting in 5 groups (cluster 1 to 5) for July and 5 for December

#3.1 choose the month
#--------------------
month = "07"
month = "12"

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


#rename the colors to new color palette
if(month == "07"){
  #red -> ochre
  #green -> yellow
  #blue -> grey
  #yellow -> darkblue
  #grey -> lightblue
  
  #following the pattern: new = old
  df_grps <- dplyr::mutate(df_grps, group = fct_recode(df_grps$group,"ochre" = "red", "yellow" = "green", "grey" = "blue", "darkblue" = "yellow", "lightblue" = "grey"))
}else if(month == "12"){
  #red (orange) -> darkblue
  #blue -> lightblue
  #green -> yellow
  #grey -> ochre
  #yellow (turquoise) -> grey
  
  df_grps <- dplyr::mutate(df_grps, group = fct_recode(df_grps$group,"darkblue" = "red", "lightblue" = "blue", "yellow" = "green", "ochre" = "grey", "grey" = "yellow"))
}

#3.4 Statistical evaluation
#--------------------------
#3.4.1 plot
#----------
if(month == "07"){
  monthName <- "Juli"
}else if(month == "12"){
  monthName <- "Dezember"
}

#boxplots
plot(df_grps$group, df_grps$precip,
     xlab = "Cluster-Klassen",
     ylab = "Niederschlag in mm",
     main = paste0("Niederschlagswerte in den einzelnen Wolkenklassen\n",monthName),
     outline = F)

#violine plot
if(month == "12"){
  ggplot(df_grps, aes(x=group, y=precip, fill=group))+
    #adjust corrects the distribution for the non-continuous precipitation data 
    #in 1/10mm steps
    geom_violin(adjust = 3)+
    scale_fill_manual(values = c("#586CC3", "#EBEA76","#BAC090", "#0044A3", "#8A97AA"))+
    stat_summary(fun=mean, geom="point", shape=3, size=5, color="red", fill="red")+
    theme(legend.position = "none")+
    scale_x_discrete(breaks = c("lightblue", "yellow", "ochre", "darkblue", "grey"),
                     labels = c("Hellblau", "Gelb", "Ocker", "Dunkelblau", "Grau"))+
    xlab("Clustergruppen")+
    ylab("Niederschlag in mm")+
    ggtitle(paste0("Niederschlagsverteilung in den Clustergruppen im ", monthName))
}else if(month == "07"){
  ggplot(df_grps, aes(x=group, y=precip, fill=group))+
    #adjust corrects the distribution for the non-continuous precipitation data 
    #in 1/10mm steps
    geom_violin(adjust = 3)+
    scale_fill_manual(values = c("#8A97AA", "#EBEA76","#586CC3", "#BAC090", "#0044A3"))+
    stat_summary(fun=mean, geom="point", shape=3, size=5, color="red", fill="red")+
    theme(legend.position = "none")+
    scale_x_discrete(breaks = c("lightblue", "yellow", "ochre", "darkblue", "grey"),
                     labels = c("Hellblau", "Gelb", "Ocker", "Dunkelblau", "Grau"))+
    #ylim(0,2)+
    xlab("Clustergruppen")+
    ylab("Niederschlag in mm")+
    ggtitle(paste0("Niederschlagsverteilung in den Clustergruppen im ", monthName))
}

#3.4.2 Mean analysis
#--------------------
#kruskal wallis test, if significance p < 0.05 -> significant differences
kruskal.test(precip ~ group, data = df_grps)

#dunns test if kw-test is significant
dunnResult <- FSA::dunnTest(precip ~ group, data = df_grps, method = "bh")
dunnResult

#3.4.2.1 Results July
#---------------------
#              Comparison           Z       P.unadj         P.adj
# 1       darkblue - grey  -5.2181852  1.806847e-07  3.011412e-07
# 2  darkblue - lightblue  24.4795210 2.441145e-132 6.102862e-132
# 3      grey - lightblue  28.2493906 1.448089e-175 7.240446e-175
# 4      darkblue - ochre  -5.5025136  3.744144e-08  7.488288e-08
# 5          grey - ochre   0.3933848  6.940353e-01  7.711504e-01
# 6     lightblue - ochre -30.7060254 4.729441e-207 4.729441e-206
# 7     darkblue - yellow  -4.3746169  1.216459e-05  1.737799e-05
# 8         grey - yellow   0.6015328  5.474852e-01  6.843565e-01
# 9    lightblue - yellow -26.7596119 9.544379e-158 3.181460e-157
# 10       ochre - yellow   0.2935336  7.691143e-01  7.691143e-01

print("ochre")
summary(df_grps$precip[df_grps$group == "ochre"])
print("yellow")
summary(df_grps$precip[df_grps$group == "yellow"])
print("grey")
summary(df_grps$precip[df_grps$group == "grey"])
print("darkblue")
summary(df_grps$precip[df_grps$group == "darkblue"])
print("lightblue")
summary(df_grps$precip[df_grps$group == "lightblue"])

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1000  0.1000  0.2000  0.2269  0.3000  5.3000 ochre
# 0.1000  0.1000  0.2000  0.2317  0.3000  5.8000 yellow
# 0.1000  0.1000  0.2000  0.2367  0.3000  5.9000 grey
# 0.1000  0.1000  0.1000  0.2286  0.3000  6.8000 darkblue
# 0.1000  0.1000  0.1000  0.2148  0.2000  6.6000 lightblue

#3.4.2.1 Results December
#-------------------------
#              Comparison         Z       P.unadj         P.adj
# 1       darkblue - grey -4.626044  3.727156e-06  4.658945e-06
# 2  darkblue - lightblue -7.024731  2.144791e-12  3.063987e-12
# 3      grey - lightblue -3.304727  9.506899e-04  1.056322e-03
# 4      darkblue - ochre  2.834580  4.588595e-03  4.588595e-03
# 5          grey - ochre  8.558776  1.140718e-17  1.901196e-17
# 6     lightblue - ochre 10.861360  1.761069e-27  3.522139e-27
# 7     darkblue - yellow 14.924410  2.286479e-50  7.621595e-50
# 8         grey - yellow 23.785842 4.680229e-125 2.340114e-124
# 9    lightblue - yellow 24.949736 2.149515e-137 2.149515e-136
# 10       ochre - yellow 13.100029  3.291319e-39  8.228297e-39

print("ochre")
summary(df_grps$precip[df_grps$group == "ochre"])
print("yellow")
summary(df_grps$precip[df_grps$group == "yellow"])
print("grey")
summary(df_grps$precip[df_grps$group == "grey"])
print("darkblue")
summary(df_grps$precip[df_grps$group == "darkblue"])
print("lightblue")
summary(df_grps$precip[df_grps$group == "lightblue"])

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1000  0.1000  0.1000  0.1383  0.1000  1.1000 ochre
# 0.1000  0.1000  0.1000  0.1262  0.1000  1.1000 yellow
# 0.1000  0.1000  0.1000  0.1447  0.2000  0.9000 grey
# 0.1000  0.1000  0.1000  0.1392  0.2000  1.0000 darkblue
# 0.1000  0.1000  0.1000  0.1461  0.2000  1.0000 lightblue

#3.4.3 Save results
#------------------
write.csv(dunnResult$res, paste0(workingDir, "07_Ergebnisse/DunnResult_clusterAnalysis_perNewGroup_", str_sub(monthName, 1, 3), ".csv"), row.names = F)



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

#4.1 load the data like above
#----------------------------
# month = "07"
# #month = "12"
# 
# list <- list.files(paste0(dataDir, "DaySlicing_newGroups"))
# list <- list[str_sub(list, 24,25) == month]
# 
# #4.2 prepare a df and predefinings
# #---------------------------------
# #predefinings 
# clusGroup <- c("red", "green", "blue", "yellow", "grey")
# channels <- c("IR_039", "IR_087", "IR_097", "IR_108", "IR_120", "IR_134", "WV_062", "WV_073")
# 
# #three layers are necessary:
# # - month, regulated by datetime
# # - clustergroup, regulated by clustergroup name
# # - spectral channel, regulated by channel name
# rm(df_grps, df_temp, precip_clusGrp, precipVals)
# 
# df_spec <- data.frame(datetime = NA, clusterGroup = NA, specChannel = NA, 
#                       bxpl_lowWhisk = NA, bxpl_lowHin = NA, bxpl_median = NA, bxpl_higWhisk = NA, bxpl_higHin = NA)
# 
# #4.3 extract boxplot information 
# #--------------------------------
# #iterate over scenes
# for(i in 1:length(list)){
#   df_temp <- read.csv(paste0(dataDir, "DaySlicing_newGroups/", list[i]))
#   
#   #iterate over cluster elements
#   for(j in clusGroup){
#     df_clus_temp <- df_temp[df_temp$ClusterGroup_continuing == j,]
#     
#     #iterate over spectral channels
#     for(k in channels){
#       #information for df
#       datetime <- df_temp[1,1]
#       clusterGroup <- j
#       specChannel <- k
#       
#       dat <- boxplot.stats(df_clus_temp[,names(df_clus_temp) == k])[[1]]
#       if(is.na(df_spec[1,1])){
#         df_spec <- data.frame(datetime = datetime, clusterGroup = clusterGroup, specChannel = specChannel, 
#                               bxpl_lowWhisk = NA, bxpl_lowHin = NA, bxpl_median = NA, bxpl_higWhisk = NA, bxpl_higHin = NA)
#         df_spec[,4:8] <- dat
#       }else{
#         datrow <- c(datetime, clusterGroup, specChannel, dat)
#         df_spec <- rbind(df_spec, datrow)
#       }
#     }
#   }
# }
# 
# #write.csv(df_spec, paste0(dataDir, "/SpectralBoxplots_Dez.csv"), row.names = F)
# write.csv(df_spec, paste0(dataDir, "/SpectralBoxplots_Jul.csv"), row.names = F)
