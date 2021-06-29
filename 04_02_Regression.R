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
set.seed(1212)

for(file in fileList){
        print(file)
        df <- read.csv(paste0(file_base, "Intersection_CT_RD/", file))[,-1]
        #round data to radolan accuracy of 1/10mm
        #df$precipitation <- round(df$precipitation, digits = 1)
        #remove zeros (zero precipitation is set to NA by python)
        df <- df[!is.na(df$precipitation),]
        #df <- df[df$precipitation != 0,]
        df <- df[df$precipitation > 0.01,]
        
        #remove cloudtypes that aren't interesting for precipitation study
        df <- df[!is.na(df$cloudType),] #NA
        df <- df[df$cloudType != 0,] #clear
        df <- df[df$cloudType != 7,] #cirrus
        
        #draw a sample of the valid data values based on Moran's I test
        # df <- df[sample(1:nrow(df), 250),]
        # distance <- as.matrix(dist(cbind(df_sample$lon, df_sample$lat)))
        # distance <- 1/distance
        # diag(distance) <- 0
        # #removing infinity values
        # distance[is.infinite(distance)] <- 0
        # ape::Moran.I(df_sample$precipitation, distance)
        # 300 -> p = 0,0001
        # 250 -> p = 0,095
        # 200 -> p = 0,12
        # 100 -> p = 0,55
        
        #Moran's I test shows, that the null-hypothesis can be rejected for samples 
        # somewhere between 300(p = 0,0001) and 250 (p = 0,095) pixels
        #thus, a valid sample number is found with 250 pixels 
        
        #when there are less than 250 entries take all of them 
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
        if(file == fileList[1]){
                df_total <- df
        }else{
               df_total <- rbind(df_total, df)
        }
}
df <- df_total
rm(df_total)
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
# [1] 0.01755382
# 
# $expected
# [1] -0.00010001
# 
# $sd
# [1] 0.0002191169
# 
# $p.value
# [1] 0
rm(df_sample, distance)
# #-------------------------------------------------------------------------------
# #4 VISUAL TEST FOR NORMAL DISTRIBUTION
# #-------------------------------------------------------------------------------
# df_smp <- df[sample(1:nrow(df), 10000),]
# qqnorm(df_smp$precipitation[df_smp$cloudType == "water"])
# qqline(df_smp$precipitation[df_smp$cloudType == "water"], col = 2)
# 
# qqnorm(as.numeric(df_smp$cloudType))
# qqline(as.numeric(df_smp$cloudType), col = 2)
# 
# rm(df_smp)
# #-------------------------------------------------------------------------------
# #5 LINEAR REGRESSION
# #-------------------------------------------------------------------------------
# #a normal linear regression
# m <- lm(precipitation~cloudType, data = df)
#  
# summary(m)
# hist(df$precipitation)
# boxplot(precipitation~cloudType, data = df, outline = T,
#         xlab = "Cloud Type", ylab = "Precipitation", main = "Precipitation distribution over the cloud types")
# 
# #R-Squared: 5,9% (as the Adjusted R² is based on 1 = 100%)
# 
# 
# #TRANSFORM THE DATA TO NORMAL DISTRIBUTION
# #------------------------------------------
# #transformationsparameter lambda
# caret::BoxCoxTrans(df$precipitation)$lambda
# #calculate transformed precipitation values
# df$transformedVal <- df$precipitation^BoxCoxTrans(df$precipitation)$lambda
# #model with transformed /linearized values 
# m_trans <- lm(transformedVal~cloudType, data = df)
# summary(m_trans)
# 
# #R-Squared: 5,3%
# rm(m, m_trans)
# 
# #-------------------------------------------------------------------------------
# #6 GAM
# #-------------------------------------------------------------------------------
# #x: cloudType
# #y: precipitation
# #linear relationship (von Master, Datenanalyse VL)
# #nimmt aber Normalverteilung an, deshalb schlechtes Ergebnis
# gamMod <- gam(precipitation ~ cloudType, data = df, family = gaussian())
# summary(gamMod)
# #--------------------------
# #explained deviance: 5,87%
# #---------------------------
# 
# #BIG GAM WITH LAT LON FOR CORRECTION OF AUTOCORRELATION AS REGIONAL SPLINES---
# #big gam = bam
# gamMod <- bam(precipitation ~ cloudType, data = df, family = gaussian())
# #give the same results as gam()
# summary(gamMod)
# 
# #subsample to reduce calculating time
# #df <- df[sample(1:nrow(df), 300000),]
# gamMod <- bam(precipitation ~ cloudType + te(lat, lon), data = df, family = gaussian())
# summary(gamMod)
# #gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 50), data = df, family = gaussian())
# gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = gaussian())
# summary(gamMod)
# 
# gamMod <- bam(precipitation ~ cloudType + te(lat, lon), data = df, family = Gamma())
# summary(gamMod)
# 
# gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = Gamma())
# summary(gamMod)
# 
# #--------------------------
# #explained deviance: 5,87% (BAM)
# #explained deviance: 6.66% (BAM WITH LAT,LON, k = NULL, Gaussian Dist.)
# #explained deviance: 5,75% (BAM WITH LAT,LON, k = 10,   Gaussian Dist.)
# #explained deviance: 6,01% (BAM WITH LAT,LON, k = 15,   Gaussian Dist.)
# #explained deviance: 11,1% (BAM WITH LAT,LON, k = NULL, Gamma Dist.)
# #explained deviance: "11,1%" (BAM WITH LAT,LON, k = 10,   Gamma Dist.) ERROR
# #explained deviance: "11,1%" (BAM WITH LAT,LON, k = 15,   Gamma Dist.) ERROR
# #---------------------------
# 
# rm(gamMod)
# 
# # df <- df[sample(1:nrow(df), 10000),]
# # 
# # #using one regression spline
# # #spezieller Spline für LatLon da die beiden Zusamenhängen
# # gamm_spatial <- gamm(precipitation ~ cloudType + s(lat) + s(lon),
# #                      data = df,
# #                      correlation = corSpatial(form = ~ lon + lat, type = "gaussian"))
# # 
# # gammod <- gam(precipitation ~ cloudType + s(lat, fx = FALSE), data = df)
# 
# #-------------------------------------------------------------------------------
# #7 GAMMA DISTRIBUTION
# #-------------------------------------------------------------------------------
# beta = var(df$precipitation)/mean(df$precipitation)
# alpha = mean(df$precipitation)/beta
# avrg = alpha*beta
# stdDev = sqrt(alpha*beta^2)
# x = 50
# range = seq(from = 0.1, to = 1, by = 0.001)
# y = dgamma(range, alpha, rate = 1/beta)
# plot(range, y, type = "l", ylim = c(0,8))
# hist(df$precipitation, prob = T,xaxt = "n", xlim = c(0.03,1), main = "Precipitation distribution\nwith Gamma Distribution in red")
# axis(side = 1, at = seq(0.1, 1, 0.1))
# lines(range, y, type = "l", col = "red", lwd = 2)
# 
# rm(y, x, range, stdDev, alpha, beta)

#-------------------------------------------------------------------------------
#8 KRUSKAL-WALLIS-TEST AND DUNN'S TEST
#-------------------------------------------------------------------------------
kruskal.test(precipitation ~ cloudType, data = df)
#Kruskal-Wallis chi-squared = 1076.8, df = 4, p-value < 2.2e-16
#-> there are significant differences between the groups as p < 0.05

dunnResult <- FSA::dunnTest(precipitation ~ cloudType, data = df, method = "bh")
dunnResult
# "*" ...significant (p < 0.05)
# "-" ...not significant

#for n = 10.000 samples
# Comparison                     Z         P.unadj       P.adj(Benjamini-Hochberg adjustmen)
# 1        opaque_ice - overlap  13.419472 4.650163e-41 5.166848e-41*
# 2   opaque_ice - overshooting -96.142495 0.000000e+00 0.000000e+00*
# 3      overlap - overshooting -89.852600 0.000000e+00 0.000000e+00*
# 4    opaque_ice - supercooled 142.825774 0.000000e+00 0.000000e+00*
# 5       overlap - supercooled  71.387839 0.000000e+00 0.000000e+00*
# 6  overshooting - supercooled 171.373717 0.000000e+00 0.000000e+00*
# 7          opaque_ice - water  56.878806 0.000000e+00 0.000000e+00*
# 8             overlap - water  42.478584 0.000000e+00 0.000000e+00*
# 9        overshooting - water 110.629264 0.000000e+00 0.000000e+00*
# 10        supercooled - water   1.729364 8.374403e-02 8.374403e-02

#*... significant

#for n = 250 samples
# Comparison                     Z         P.unadj       P.adj(Benjamini-Hochberg adjustmen)
# 1        opaque_ice - overlap   1.6733632  9.425583e-02  1.047287e-01
# 2   opaque_ice - overshooting -15.7318136  9.155083e-56  2.288771e-55*
# 3      overlap - overshooting -14.3350539  1.321440e-46  2.642880e-46*
# 4    opaque_ice - supercooled  21.7077130 1.734774e-104 8.673868e-104*
# 5       overlap - supercooled  11.1597635  6.416205e-29  1.069368e-28*
# 6  overshooting - supercooled  27.1539190 2.276096e-162 2.276096e-161*
# 7          opaque_ice - water   9.2947949  1.474904e-20  2.107005e-20*
# 8             overlap - water   7.1149684  1.119385e-12  1.399231e-12*
# 9        overshooting - water  18.3511234  3.233573e-75  1.077858e-74*
# 10        supercooled - water   0.5374891  5.909298e-01  5.909298e-01

#*... significant

#mean values
print("overlap")
summary(df$precipitation[df$cloudType == "overlap"])
print("opaque_ice")
summary(df$precipitation[df$cloudType == "opaque_ice"])
print("supercooled")
summary(df$precipitation[df$cloudType == "supercooled"])
print("overshooting")
summary(df$precipitation[df$cloudType == "overshooting"])
print("water")
summary(df$precipitation[df$cloudType == "water"])

#comparison
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
