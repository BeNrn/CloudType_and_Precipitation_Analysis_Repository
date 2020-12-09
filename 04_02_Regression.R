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
# [1] 0.02580478
# 
# $expected
# [1] -0.00010001
# 
# $sd
# [1] 0.0002310415
# 
# $p.value
# [1] 0
rm(df_sample, distance)
#-------------------------------------------------------------------------------
#4 VISUAL TEST FOR NORMAL DISTRIBUTION
#-------------------------------------------------------------------------------
df_smp <- df[sample(1:nrow(df), 10000),]
qqnorm(df_smp$precipitation[df_smp$cloudType == "water"])
qqline(df_smp$precipitation[df_smp$cloudType == "water"], col = 2)

qqnorm(as.numeric(df_smp$cloudType))
qqline(as.numeric(df_smp$cloudType), col = 2)

rm(df_smp)
#-------------------------------------------------------------------------------
#5 LINEAR REGRESSION
#-------------------------------------------------------------------------------
#a normal linear regression
m <- lm(precipitation~cloudType, data = df)
 
summary(m)
hist(df$precipitation)
boxplot(precipitation~cloudType, data = df, outline = T,
        xlab = "Cloud Type", ylab = "Precipitation", main = "Precipitation distribution over the cloud types")

#R-Squared: 4,6% (as the Adjusted R² is based on 1 = 100%)


#TRANSFORM THE DATA TO NORMAL DISTRIBUTION
#------------------------------------------
#transformationsparameter lambda
caret::BoxCoxTrans(df$precipitation)$lambda
#calculate transformed precipitation values
df$transformedVal <- df$precipitation^BoxCoxTrans(df$precipitation)$lambda
#model with transformed /linearized values 
m_trans <- lm(transformedVal~cloudType, data = df)
summary(m_trans)

#R-Squared: 3,0%
rm(m, m_trans)

#-------------------------------------------------------------------------------
#6 GAM
#-------------------------------------------------------------------------------
#x: cloudType
#y: precipitation
#linear relationship (von Master, Datenanalyse VL)
#nimmt aber Normalverteilung an, deshalb schlechtes Ergebnis
gamMod <- gam(precipitation ~ cloudType, data = df, family = gaussian())
summary(gamMod)
#--------------------------
#explained deviance: 4,63%
#---------------------------

#BIG GAM WITH LAT LON FOR CORRECTION OF AUTOCORRELATION AS REGIONAL SPLINES---
#big gam = bam
gamMod <- bam(precipitation ~ cloudType, data = df, family = gaussian())
#give the same results as gam()
summary(gamMod)

#subsample to reduce calculating time
#df <- df[sample(1:nrow(df), 300000),]
gamMod <- bam(precipitation ~ cloudType + te(lat, lon), data = df, family = gaussian())
summary(gamMod)
#gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 50), data = df, family = gaussian())
gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = gaussian())
summary(gamMod)

gamMod <- bam(precipitation ~ cloudType + te(lat, lon), data = df, family = Gamma())
summary(gamMod)

gamMod <- bam(precipitation ~ cloudType + te(lat, lon, k = 15), data = df, family = Gamma())
summary(gamMod)

#--------------------------
#explained deviance: 4,63% (BAM)
#explained deviance: 5,32% (BAM WITH LAT,LON, k = NULL, Gaussian Dist.)
#explained deviance: 5,75% (BAM WITH LAT,LON, k = 10,   Gaussian Dist.)
#explained deviance: 6,01% (BAM WITH LAT,LON, k = 15,   Gaussian Dist.)
#explained deviance: 11,1% (BAM WITH LAT,LON, k = NULL, Gamma Dist.)
#explained deviance: "11,1%" (BAM WITH LAT,LON, k = 10,   Gamma Dist.) ERROR
#explained deviance: "11,1%" (BAM WITH LAT,LON, k = 15,   Gamma Dist.) ERROR
#---------------------------

rm(gamMod)

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

rm(y, x, range, stdDev, alpha, beta)
#-------------------------------------------------------------------------------
#8 KRUSKAL-WALLIS-TEST
#-------------------------------------------------------------------------------
kruskal.test(precipitation ~ cloudType, data = df)
#-> there are significant differences between the groups as p < 0.05

dunnResult <- FSA::dunnTest(precipitation ~ cloudType, data = df, method = "bh")
dunnResult
#                 Comparison          Z      P.unadj        P.adj
# 1     opaque_ice - overlap -1.0568259 2.905910e-01 3.487092e-01 <- not significant
# 2 opaque_ice - supercooled -3.2768600 1.049684e-03 1.574526e-03 <- significant, although water group is not part of it
# 3    overlap - supercooled -0.5284487 5.971879e-01 5.971879e-01 <- not significant
# 4       opaque_ice - water  8.6537070 4.985309e-18 1.495593e-17 <- water is quite different from the other groups
# 5          overlap - water  7.6815953 1.571196e-14 3.142391e-14 <-  -"-
# 6      supercooled - water 11.3748885 5.577411e-30 3.346447e-29 <-  -"-

# "*" ...significant
# "-" ...not significant

#   Comparison                   Z          P.unadj       P.adj (Benjamini-Hochberg adjustmen)
# 1        opaque_ice - overlap  -8.385532  5.049669e-17  6.312086e-17 *
# 2   opaque_ice - overshooting -83.608021  0.000000e+00  0.000000e+00 *
# 3      overlap - overshooting -70.014296  0.000000e+00  0.000000e+00 *
# 4    opaque_ice - supercooled  47.453424  0.000000e+00  0.000000e+00 *
# 5       overlap - supercooled  36.619214 1.414674e-293 2.357791e-293 *
# 6  overshooting - supercooled 102.365663  0.000000e+00  0.000000e+00 *
# 7          opaque_ice - water  -5.743477  9.275187e-09  1.030576e-08 *
# 8             overlap - water  -1.992442  4.632259e-02  4.632259e-02 * (almost not)
# 9        overshooting - water  43.268998  0.000000e+00  0.000000e+00 *
# 10        supercooled - water -17.528444  8.691519e-69  1.241646e-68 *

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
