#this skript provides the evaluation of the results of the cluster analysis

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/")

library(FSA)
#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
list <- list.files(paste0(dataDir, "ClusterAnalysis"))
list <- list[endsWith(list, ".csv")]
df <- read.csv(paste0(dataDir, "Clusteranalysis/", list[1]))
df$ClusterGroup <- as.factor(df$ClusterGroup)

#statistics boxplot
boxplot(df$precipitation[df$ClusterGroup == 1], ylim = c(0,1.4))
summary(df$precipitation[df$ClusterGroup == 1])

kruskal.test(precipitation ~ ClusterGroup, data = df)
#p-value = 0.02674
#p-value < 0.05

dunnResult <- FSA::dunnTest(precipitation ~ ClusterGroup, data = df, method = "bh")
dunnResult
