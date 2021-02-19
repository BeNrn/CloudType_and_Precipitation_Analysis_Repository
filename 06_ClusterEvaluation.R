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
#month = "07"
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
    geom_violin(adjust = 1)+
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
    geom_violin(adjust = 1)+
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
#   Comparison            Z           P.unadj      P.adj
# 1       darkblue - grey -0.28956124 7.721519e-01 9.651899e-01
# 2  darkblue - lightblue -0.07096725 9.434238e-01 9.434238e-01
# 3      grey - lightblue  0.21139451 8.325794e-01 9.250883e-01
# 4      darkblue - ochre -2.94255980 3.255109e-03 6.510218e-03
# 5          grey - ochre -2.62664504 8.623123e-03 1.231875e-02
# 6     lightblue - ochre -2.79169363 5.243298e-03 8.738829e-03
# 7     darkblue - yellow  8.41898466 3.797611e-17 1.265870e-16
# 8         grey - yellow  8.62421581 6.453332e-18 3.226666e-17
# 9    lightblue - yellow  8.26645577 1.380002e-16 3.450006e-16
# 10       ochre - yellow 11.24298862 2.507471e-29 2.507471e-28

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
#0.01000 0.03678 0.08795 0.15652 0.20058 6.84922 ochre
#0.01000 0.03360 0.08117 0.15516 0.19488 5.37895 yellow
#0.01000 0.03635 0.08645 0.15285 0.19740 5.91566 grey
#0.01000 0.03633 0.08604 0.15498 0.19765 5.28729 darkblue
#0.01000 0.03518 0.08632 0.15867 0.20313 6.62324 lightblue

#3.4.2.1 Results December
#-------------------------
#   Comparison             Z          P.unadj       P.adj
# 1       darkblue - grey -35.9343579 8.884280e-283 2.221070e-282
# 2  darkblue - lightblue -19.1737395  6.133707e-82  1.022285e-81
# 3      grey - lightblue  16.8661981  7.976811e-64  9.971014e-64
# 4      darkblue - ochre  16.5446258  1.750490e-61  1.944989e-61
# 5          grey - ochre  59.4865667  0.000000e+00  0.000000e+00
# 6     lightblue - ochre  38.9761051  0.000000e+00  0.000000e+00
# 7     darkblue - yellow  -0.2502198  8.024174e-01  8.024174e-01
# 8         grey - yellow  39.6792192  0.000000e+00  0.000000e+00
# 9    lightblue - yellow  20.7566772  1.066938e-95  2.133876e-95
# 10       ochre - yellow -18.4875135  2.602824e-76  3.718320e-76

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
# 0.01000 0.02086 0.04148 0.06767 0.08363 1.10616 ochre
# 0.01000 0.02717 0.05034 0.06947 0.08794 1.09407 yellow
# 0.01000 0.03344 0.06810 0.09533 0.12454 0.92635 grey
# 0.01000 0.02341 0.04890 0.07597 0.10045 0.99557 darkblue
# 0.01000 0.02877 0.05839 0.08693 0.11420 1.05766 lightblue

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
