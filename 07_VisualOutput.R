#skript of visual output production

workingDir <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/"
dataDir <- paste0(workingDir, "03_Data/")

library(raster)
library(rgdal)
library(magrittr)
library(stringr)
library(ggplot2)

#-------------------------------------------------------------------------------
#1 ClusterAnalysis - WRITING AS TIFF
#-------------------------------------------------------------------------------
list <- list.files(paste0(dataDir, "ClusterAnalysis/DaySlicing/"))
list <- list[endsWith(list, ".csv")]
#list <- c(list[1:2], list[4:5], list[11:14])
#list <- list[7:10]

#iterate
for(listelement in list){
  #read data
  df <- read.csv(paste0(dataDir, "ClusterAnalysis/DaySlicing/",as.character(listelement)))
  #df to spatial points data frame
  coordinates(df) <- ~ lon + lat
  #spdf to spatial pixels data frame
  gridded(df) <- TRUE
  #to raster
  df_rast <- stack(df)
  projection(df_rast) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" 
  #save
  #writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/01_07_2017_13Uhr_4klassen", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
  #writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/12_12_2017_02Uhr_", str_sub(listelement, 25,33), ".tif"), format = "GTiff", overwrite = TRUE)
  writeRaster(df_rast, paste0(dataDir, "ClusterAnalysis/DaySlicing_Tiff/", listelement %>% str_sub(1,16),"_", listelement %>% str_sub(25,33), ".tif"), format = "GTiff", overwrite = TRUE)
}

#-------------------------------------------------------------------------------
#2 ClusterAnalysis - BOXPLOT ANALYSIS
#-------------------------------------------------------------------------------
df <- read.csv(paste0(dataDir, "ClusterAnalysis/SpectralBoxplots_Jul.csv"))
df_sceneSlice <- df[df$datetime == "2017-07-12 00:00",]
df_colSlice <- df_sceneSlice[df_sceneSlice$clusterGroup == "red",]

ggplot(data = df_colSlice)+
  geom_point(mapping = aes(x = specChannel, y = bxpl_median), col = "red")


#-------------------------------------------------------------------------------
#3 VALUE RANGE - ASSIGNING CT AND ISCCP
#-------------------------------------------------------------------------------
#data see 00-Aktueller_Stand_MA

df <- read.csv(paste0(dataDir, "Zuordnung_CT_ISCCP.csv"), sep = ";", dec = ",")
#increase visibility of points
for(rows in seq(1:nrow(df))){
  if(df$min_val[rows] == df$max_val[rows]){
    df$min_val[rows] <- df$min_val[rows] - 0.2
    df$max_val[rows] <- df$max_val[rows] + 0.2
  }
}
  
#duplicate the values for "boxplot" generation
df <- rbind(df, df)

df$groupID <- paste0(df$cloudType, "-", df$source)
df$parameter2 <- NA

#duplicate again to rebuild df in true long format
df <- rbind(df, df)
df$parameter2[1:74] <- "max"
df$parameter2[75:148] <- "min"

#fill long format value column
df$lon_val <- NA
df$lon_val[df$parameter2 == "max"] <- df$max_val[1:74]
df$lon_val[df$parameter2 == "min"] <- df$min_val[1:74]

#remove unnecessary cols
df$min_val <- NULL
df$max_val <- NULL



df_test <- df[df$parameter == "cloudTop_span",]
#df_test <- df_test[df_test$cloudType == "sc",]
#filter(source %in% unique(df_test$source)) %>%
ggplot(df_test, aes(x=cloudType, y = lon_val, fill = source)) + 
  geom_boxplot(fatten = NULL, width = 0.3, color = "NA" )+
  xlab("")+
  ylab("Temperatur in °C")+
  ggtitle("Wolkenobergrenzentemperatur je Wolkenklasse nach den entsprechenden Quellen")+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values = c("#364DD7", "#4E64CA", "#687ABA", "#8290AD", "#C1C78B", "#E5E478", "#E7C160", "#EAA860"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df[df$parameter == "cloudTop_span",], aes(x=groupID, y = lon_val, group = cloudType)) + 
  geom_boxplot()+
  xlab("")+
  ylab("Temperatur in °C")+
  theme_light()

  geom_boxplot(fatten = NULL, position = "identity", fill = "slateblue", color = NA, alpha = 0.3)+
  theme_light()

# temperature <- data.frame(temp = c(265, 265,275, 275), position = "temp", class = "cu_temp")
# tmp <- data.frame(temp = c(270, 270,277, 277), position = "temp", class = "cu_temp2")
# temperature <- rbind(temperature, tmp)

# ggplot(temperature, aes(x=temp, y = position, group = class)) + 
#   geom_boxplot(fatten = NULL, position = "identity", fill = "slateblue", color = "white", alpha = 0.3)+
#   theme_light()

temp_cu <- data.frame(temp = c(265, 265,275, 275), position = "cu_temp", class = "cu_temp")

temp_sc <- data.frame(temp = c(275, 275, 277, 277), position = "sc_temp", class = "sc_temp")

temp_st <- data.frame(temp = c(263, 263, 287, 287), position = "st_temp", class = "st_temp")
temp_st2 <- data.frame(temp = c(275, 275, 277, 277), position = "st_temp", class = "st_temp2")

temp_ac <- data.frame(temp = c(247.15, 247.15, 261.15, 261.15), position = "ac_temp", class = "ac_temp")
temp_ac2 <- data.frame(temp = c(215, 215, 260, 260), position = "ac_temp", class = "ac_temp2")
temp_ac3 <- data.frame(temp = c(211, 211, 253, 253), position = "ac_temp", class = "ac_temp3")

temp_as <- data.frame(temp = c(215, 215, 260, 260), position = "as_temp", class = "as_temp")
temp_as2 <- data.frame(temp = c(211, 211, 277, 277), position = "as_temp", class = "as_temp2")

temperature <- rbind(temp_cu, temp_sc, temp_st, temp_st2, temp_ac, temp_ac2, temp_ac3)

ggplot(temperature, aes(x=temp, y = position, group = class)) + 
  geom_boxplot(fatten = NULL, position = "identity", fill = "slateblue", color = NA, alpha = 0.3)+
  theme_light()

             