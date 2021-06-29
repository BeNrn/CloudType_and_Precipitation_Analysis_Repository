#this skript provides the evaluation of the results of the cluster analysis

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/ClusterAnalysis/")

library(FSA)
library(stringr)
library(magrittr)
library(forcats)
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
#filter for 5 clustering groups
list <- list[str_sub(list, 9,9) == "5"]

for(j in 1:length(list)){
  df <- read.csv(paste0(dataDir, "DaySlicing/", list[j]))
  
  #-----------------------------------------------------------------------------
  #2 ASSIGN CONTINUING CLUSTER GROUP
  #-----------------------------------------------------------------------------
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
      #write.csv(df, paste0(dataDir, "/DaySlicing_NewGroups/Cluster_newGroups_", daytime, ".csv"), row.names = F)
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
#month = "07"
month = "12"


#3.2 load the cluster groups
#---------------------------
clusGroup <- c("red", "green", "blue", "yellow", "grey")

#3.3 load and prepare the data
#------------------------------
list <- list.files(paste0(dataDir, "DaySlicing_newGroups"))
list <- list[str_sub(list, 24,25) == month]

#December is split in two groups, only the group from 9-15 o'clock is clearly
# separable
if(month == "12"){
  list <- list[1:25] 
}

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
  #red -> darkblue
  #green -> lightblue
  #blue -> ochre
  #yellow -> yellow
  #grey -> grey
  
  #following the pattern: new = old
  df_grps <- dplyr::mutate(df_grps, group = fct_recode(df_grps$group,"ochre" = "blue", "yellow" = "yellow", "grey" = "red", "darkblue" = "grey", "lightblue" = "green"))
}else if(month == "12"){
  #red -> grey
  #blue -> ochre
  #green -> lightblue
  #grey -> darkblue
  #yellow -> yellow
  
  df_grps <- dplyr::mutate(df_grps, group = fct_recode(df_grps$group,"darkblue" = "red", "lightblue" = "green", "yellow" = "yellow", "ochre" = "blue", "grey" = "grey"))
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
    geom_violin(adjust = 1)+
    scale_fill_manual(values = c("#BAC090", "#586CC3", "#8A97AA", "#0044A3", "#EBEA76"))+
    stat_summary(fun=mean, geom="point", shape=3, size=5, color="red", fill="red")+
    theme(legend.position = "none")+
    scale_x_discrete(breaks = c("lightblue", "yellow", "ochre", "darkblue", "grey"),
                     labels = c("Hellblau", "Gelb", "Ocker", "Dunkelblau", "Grau"))+
    xlab("Clustergruppen")+
    ylab("Niederschlag in mm")+
    ggtitle(paste0("Niederschlagsverteilung in den Clustergruppen im ", monthName))
}else if(month == "07"){
  ggplot(df_grps, aes(x=group, y=precip, fill=group))+
    geom_violin(adjust = 1)+
    scale_fill_manual(values = c("#BAC090","#586CC3","#0044A3", "#8A97AA", "#EBEA76"))+
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
#Kruskal-Wallis chi-squared = 4824, df = 4, p-value < 2.2e-16

#dunns test if KW-test is significant
dunnResult <- FSA::dunnTest(precip ~ group, data = df_grps, method = "bh")
dunnResult

#3.4.2.1 Results July
#---------------------
# Comparison               Z         P.unadj       P.adj
# 1       darkblue - grey   28.47034 2.729445e-178 3.411806e-178*
# 2  darkblue - lightblue  100.83885  0.000000e+00  0.000000e+00*
# 3      grey - lightblue   69.77129  0.000000e+00  0.000000e+00*
# 4      darkblue - ochre  -36.91975 2.227717e-298 3.182453e-298*
# 5          grey - ochre  -62.81774  0.000000e+00  0.000000e+00*
# 6     lightblue - ochre -133.88356  0.000000e+00  0.000000e+00*
# 7     darkblue - yellow   15.31493  6.077759e-53  6.753065e-53*
# 8         grey - yellow  -12.50055  7.413880e-36  7.413880e-36*
# 9    lightblue - yellow  -82.06597  0.000000e+00  0.000000e+00*
# 10       ochre - yellow   49.79238  0.000000e+00  0.000000e+00*

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


# Min.   1st Qu. Median  Mean    3rd Qu. Max. 
#0.01000 0.04805 0.10630 0.17124 0.22454 5.28729 ochre
#0.01000 0.03555 0.08308 0.15468 0.19667 5.82669 yellow
#0.01000 0.03181 0.07770 0.15391 0.19431 5.91566 grey
#0.01000 0.03994 0.09017 0.15941 0.19936 6.84922 darkblue
#0.01000 0.02280 0.05077 0.11814 0.13053 6.62324 lightblue

#3.4.2.1 Results December
#-------------------------
# Comparison              Z           P.unadj       P.adj
# 1       darkblue - grey -18.798708  7.737879e-79  1.105411e-78*
# 2  darkblue - lightblue -27.024485 7.621086e-161 1.524217e-160*
# 3      grey - lightblue  -6.765316  1.330187e-11  1.662734e-11*
# 4      darkblue - ochre -22.286458 5.000217e-110 8.333695e-110*
# 5          grey - ochre  -4.426943  9.557788e-06  1.061976e-05*
# 6     lightblue - ochre   1.669299  9.505822e-02  9.505822e-02
# 7     darkblue - yellow -66.494017  0.000000e+00  0.000000e+00*
# 8         grey - yellow -45.729569  0.000000e+00  0.000000e+00*
# 9    lightblue - yellow -42.113579  0.000000e+00  0.000000e+00*
# 10       ochre - yellow -38.412842  0.000000e+00  0.000000e+00*

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

#9 - 15 Uhr
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.01000 0.02487 0.05173 0.07789 0.10377 1.05862 ochre
# 0.01000 0.03464 0.07156 0.09760 0.12928 0.92635 yellow
# 0.01000 0.02560 0.05068 0.07199 0.09420 0.89095 grey
# 0.01000 0.02132 0.04213 0.06883 0.08433 1.10616 darkblue
# 0.01000 0.02751 0.05251 0.07577 0.09652 1.09407 lightblue

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
