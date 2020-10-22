file_base <- "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"

library(forcats)
library(caret)
library(mgcv)
library(ape)
library(FSA)

#-------------------------------------------------------------------------------
#1 LOAD THE DATA
#-------------------------------------------------------------------------------
fileList <- list.files(paste0(file_base, "Intersection_CT_RD/"))
#fileList <- fileList[1]

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
#2 PLOT THE DATA
#-------------------------------------------------------------------------------

#2.1. General data
#------------------
hist(df$precipitation)

plot(df$cloudType, df$precipitation,
     xlab = "Wolkenklassen",
     ylab = "Niederschlag in mm",
     main = paste0("Niederschlagswerte in den einzelnen Wolkenklassen\n(n = ", as.character(nrow(df)), ")\nDez17 komplett - 10.000 Sample pro Tag"),
     log = "y",
     outline = F)

#2.2 Differences in a cloud type depending on the weather situation
#-------------------------------------------------------------------
op <- par()
par(mfrow = c(3,3))
for(i in seq(1:length(unique(df$weather)))){
        boxplot(df$precipitation[df$cloudType == "supercooled" & df$weather == unique(df$weather)[i]], ylim = c(0,1.4), main = as.character(unique(df$weather)[i]))
}

par(op)

#2.3 Differences between the cloud types
#---------------------------------------
#boxplot, histogram and bxplot-value extraction for all cloud types
for(i in seq(1:length(unique(df$cloudType)))){
        #the cloud type name
        ct_name <- as.character(unique(df$cloudType)[i])
        title_name <- paste0("Cloud type ",ct_name, "\nn = ", as.character(length(df$precipitation[df$cloudType == ct_name])))
        #histogram
        hist(df$precipitation[df$cloudType == ct_name],
             xlim = c(0,1.5), 
             main = title_name)
        #boxplot
        boxplot(df$precipitation[df$cloudType == ct_name], 
                main = title_name,
                ylim = c(0,3))
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

#single extraction
hist(df$precipitation[df$cloudType == "overlap"], xlim = c(0,1.5), main = paste0("Cloud type Overlap\nn = ", as.character(length(df$precipitation[df$cloudType == "overlap"]))))
boxplot(df$precipitation[df$cloudType == "overlap"])
ct_ovl <- df[df$cloudType == "overlap",]
plot(as.factor(ct_ovl$weather), ct_ovl$precipitation, ylim = c(0,3), 
     main = paste0("Cloud type Overlap\nn = ", as.character(length(df$precipitation[df$cloudType == "overlap"]))),
     xlab = "Weather situation",
     ylab = "Precipitation in mm")

#-------------------------------------------------------------------------------
#3 TEST FOR AUTOCORRELATION (MORAN'S I)
#-------------------------------------------------------------------------------
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
df_sample <- df[1:10000,]
distance <- as.matrix(dist(cbind(df_sample$lon, df_sample$lat)))
distance <- 1/distance
diag(distance) <- 0
#removing infinity values
distance[is.infinite(distance)] <- 0
ape::Moran.I(df_sample$precipitation, distance)
#Null-Hypothese: Daten zufällig verteilt
#kann abgelehnt werden wenn p-Wert signifikant
#p < 0.05 -> signifikant
#Standardabweichung sd positiv
#Nullhypothese kann abgelehnt werden -> räumliche Clusterung

# $observed
# [1] 0.04935017
# 
# $expected
# [1] -0.00010001
# 
# $sd
# [1] 0.0002808298
# 
# $p.value
# [1] 0

#-------------------------------------------------------------------------------
#4 TEST FOR NORMAL DISTRIBUTION
#-------------------------------------------------------------------------------
df_smp <- df[sample(1:nrow(df), 10000),]
qqnorm(df_smp$precipitation[df_smp$cloudType == "water"])
qqline(df_smp$precipitation[df_smp$cloudType == "water"], col = 2)

qqnorm(as.numeric(df_smp$cloudType))
qqline(as.numeric(df_smp$cloudType), col = 2)

#-------------------------------------------------------------------------------
#5 LINEAR REGRESSION
#-------------------------------------------------------------------------------
#eine normale lineare Regression
m <- lm(precipitation~cloudType, data = df)
 
summary(m)
hist(df$precipitation)
boxplot(precipitation~cloudType, data = df, outline = T,
        xlab = "Cloud Type", ylab = "Precipitation", main = "Precipitation distribution over the cloud types")

#R-Squared: 0,0008%


#TRANSFORM THE DATA TO NORMAL DISTRIBUTION
#------------------------------------------
#transformationsparameter lambda
caret::BoxCoxTrans(df$precipitation)$lambda
#calculate transformed precip values
df$transformedVal <- df$precipitation^BoxCoxTrans(df$precipitation)$lambda
#model with transformed /linearized values 
m_trans <- lm(transformedVal~cloudType, data = df)
summary(m_trans)

#R-Squared: 0,0006%

#-------------------------------------------------------------------------------
#6 GAM
#-------------------------------------------------------------------------------
#x: cloudType
#y: precipitation
#linear relationship (von Master, Datenanalyse VL)
#nimmt aber Normalverteilung an, deshalb schlechtes Ergebnis
gammod <- gam(precipitation ~ cloudType, data = df, family = gaussian())
summary(gammod)
#--------------------------
#explained deviance: 0.081%
#---------------------------

#BIG GAM WITH LAT LON FOR CORRECTION OF AUTOCORRELATION AS REGIONAL SPLINES---
#big gam = bam
gamMod <- bam(precipitation ~ cloudType, data = df, family = gaussian())
#give the same results as gam()
summary(gamMod)

#subsample to reduce calculating time
#df <- df[sample(1:nrow(df), 300000),]
#gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 50), data = df, family = gaussian())
gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = gaussian())
#, family = Gamma()
summary(gamMod)

gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = Gamma())
summary(gamMod)

#--------------------------
#explained deviance: 0.044% (BAM)
#explained deviance: 3.47% (BAM WITH LAT,LON)
#explained deviance: 6.38% (BAM WITH LAT,LON, k = 10)
#explained deviance: 8.82% (BAM WITH LAT,LON, k = 15)
#explained deviance: 4.98% (BAM WITH LAT,LON, WITH GAMMA DISTRIBUTION)
#explained deviance: 8.27% (BAM WITH LAT,LON, WITH GAMMA DISTRIBUTION, k = 10)
#explained deviance: 11% (BAM WITH LAT,LON, WITH GAMMA DISTRIBUTION, k = 15)
#---------------------------

# df <- df[sample(1:nrow(df), 10000),]
# 
# #using one regression spline
# #spezieller Spline für LatLon da die beiden Zusamenhängen
# gamm_spatial <- gamm(precipitation ~ cloudType + s(lat) + s(lon),
#                      data = df,
#                      correlation = corSpatial(form = ~ lon + lat, type = "gaussian"))
# 
# gammod <- gam(precipitation ~ cloudType + s(lat, fx = FALSE), data = df)

#-------------------------------------------------------------------------------
#7 GAMMA DISTRIBUTION
#-------------------------------------------------------------------------------
beta = var(df$precipitation)/mean(df$precipitation)
alpha = mean(df$precipitation)/beta
avrg = alpha*beta
stdDev = sqrt(alpha*beta^2)
x = 50
range = seq(from = 0.1, to = 1, by = 0.001)
y = dgamma(range, alpha, rate = 1/beta)
plot(range, y, type = "l", ylim = c(0,8))
hist(df$precipitation, prob = T,xaxt = "n", xlim = c(0.03,1), main = "Precipitation distribution\nwith Gamma Distribution in red")
axis(side = 1, at = seq(0.1, 1, 0.1))
lines(range, y, type = "l", col = "red", lwd = 2)


#test
df_smp <- df[sample(1:nrow(df), 10000),]

plot(df_smp$precipitation[df_smp$cloudType == "supercooled"])

summary(glm(precipitation~cloudType, data = df_smp, family = Gamma(link = "log")))
#summary(glm(precipitation~cloudType, data = df, family = Gamma(link = "log")))

#summary(glm(precipitation~cloudType, data = df_smp, family = Gamma))
#summary(glm(precipitation~cloudType, data = df, family = Gamma))

#https://www.theanalysisfactor.com/r-glm-model-fit/

#-------------------------------------------------------------------------------
#8 KRUSKAL-WALLIS-TEST
#-------------------------------------------------------------------------------
kruskal.test(precipitation ~ cloudType, data = df)
#-> there are significant differences between the groups as p < 0.005

dunnResult <- FSA::dunnTest(precipitation ~ cloudType, data = df, method = "bh")

#                 Comparison          Z      P.unadj        P.adj
# 1     opaque_ice - overlap -1.0568259 2.905910e-01 3.487092e-01 <- not significant
# 2 opaque_ice - supercooled -3.2768600 1.049684e-03 1.574526e-03 <- significant, although water group is not part of it
# 3    overlap - supercooled -0.5284487 5.971879e-01 5.971879e-01 <- not significant
# 4       opaque_ice - water  8.6537070 4.985309e-18 1.495593e-17 <- water is quite different from the other groups
# 5          overlap - water  7.6815953 1.571196e-14 3.142391e-14 <-  -"-
# 6      supercooled - water 11.3748885 5.577411e-30 3.346447e-29 <-  -"-

summary(df$precipitation[df$cloudType == "overlap"])
summary(df$precipitation[df$cloudType == "opaque_ice"])
plot(df$cloudType[df$cloudType == "opaque_ice" | df$cloudType == "overlap"], 
     df$precipitation[df$cloudType == "opaque_ice" | df$cloudType == "overlap"],
     )#outline = F)

summary(df$precipitation[df$cloudType == "supercooled"])
summary(df$precipitation[df$cloudType == "opaque_ice"])
plot(df$cloudType[df$cloudType == "opaque_ice" | df$cloudType == "supercooled"], 
     df$precipitation[df$cloudType == "opaque_ice" | df$cloudType == "supercooled"],
     )#outline = F)

summary(df$precipitation[df$cloudType == "supercooled"])
summary(df$precipitation[df$cloudType == "water"])
plot(df$cloudType[df$cloudType == "supercooled" | df$cloudType == "water"], 
     df$precipitation[df$cloudType == "supercooled" | df$cloudType == "water"],
     )#outline = F)
