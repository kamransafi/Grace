
# Bring in the table with the calculated metrics per day, merge the information and average them by the grace collection periods
# Use this to build a dataset and run the following models:
#_______________
# 1- monthly average of daily UD size ~ monthly grace experienced (calculated by multiplying UD probabilities * monthly grace values) - we expect larger UDs in poorer conditions (negative grace) (neg. corr)
# 2- monthly average of daily cumulative distance ~ monthly grace experienced (calculated as above) - higher daily distance in poorer conditions (negative grace) (neg. corr)
# 3- monthly "directness" of movement ~ monthly grace experienced (calculated as above) - we expect more directed movement (lower dbb variance) in poorer conditions (negative grace) (pos. corr)
#_______________

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

#___________________________________
# CREATE A DATAFRAME FOR THE MODELS

# Import table with metrics and grace experienced per day and per individual
daily_graceExp <- readRDS("MovementData/MovementMetrics/nonFlying_allDailyGraceExperienced.rds")
# and the table including individual infos, locomotory mode, daily movement metrics and ud size
daily_metrics <- readRDS("MovementData/MovementMetrics/nonFlying_metrics.rds")

# merge the two daily tables
dailyDF <- merge(daily_metrics, daily_graceExp[,-2], by="commonID", all=T)
# Create two variables for aggregation, one pasting MBid_individual and one pasting start and end of the grace collection period
#MBid_indiv <- sapply(lapply(strsplit(daily_graceExp$commonID,"_"),"[",1:2),paste,collapse="_")
MBid_indiv <- paste(dailyDF$MBid, dailyDF$individual, sep="_")
graceCollPeriod <- paste(dailyDF$start_graceCollection, dailyDF$end_graceCollection, sep="_")

# Average variables by collection period (more or less monthly)

modelDF <- aggregate(cbind(UDsizeKm2,cumulativeDist_km,motionVariance,graceExperienced,UDwMeanLongitude,UDwMeanLatitude) 
                     ~ MBid_indiv + graceCollPeriod + species + Locomotion + grace_layer + grace_seq,
                     data=dailyDF, FUN=mean)
modelDF$grace_seq <- as.numeric(modelDF$grace_seq)
modelDF$species <- as.factor(modelDF$species)

#________
# MODELS

library(mgcv)

# udMod <- gamm(log(UDsizeKm2) ~
#                 graceExperienced +
#                 s(UDwMeanLongitude,UDwMeanLatitude) + #(weighted coordinates from dayilyUD)
#                 s(grace_seq), #(time, since first measurement of Grace)
#                 #Locomotion #(locomotory.mode)
#                 random = list(species=~1), #(random fac)
#               data=modelDF,
#               na.action=na.fail) #by default
# 
# summary(udMod$gam)
# hist(residuals(udMod$gam))
# ## Extract random structure
# summary(udMod$lme)
# #intercept for each species
# udMod$lme$coefficients$random$species 
# # standard deviation of species intercepts (this is what is printed in the summary)
# RIsd=0.90

# or, similar:
udMod <- gam(log(UDsizeKm2) ~
                s(graceExperienced) +
                s(UDwMeanLongitude,UDwMeanLatitude) + #(weighted coordinates from dayilyUD)
                s(grace_seq) + #(time, since first measurement of Grace)
              #Locomotion #(locomotory.mode)
               s(species, bs="re"), #(random fac)
              data=modelDF,
             na.action=na.fail) #by default
summary(udMod)
hist(residuals(udMod))


distMod <- gam(log(cumulativeDist_km) ~
                s(graceExperienced) +
                s(UDwMeanLongitude,UDwMeanLatitude) + #(weighted coordinates from dayilyUD)
                s(grace_seq) + #(time, since first measurement of Grace)
                #Locomotion #(locomotory.mode)
                s(species, bs="re"), #(random fac)
              data=modelDF)
summary(distMod)
hist(residuals(distMod))


dirMod <- gam(log(motionVariance) ~
                s(graceExperienced) +
                s(UDwMeanLongitude,UDwMeanLatitude) + #(weighted coordinates from dayilyUD)
                s(grace_seq) + #(time, since first measurement of Grace)
                #Locomotion #(locomotory.mode)
                s(species, bs="re"), #(random fac)
              data=modelDF)
summary(dirMod)
hist(residuals(dirMod))

# diagnostic plots
par(mfrow=c(2,2))
gam.check(udMod) 
gam.check(distMod) 
gam.check(dirMod) 
# temporal autocorrelation
par(mfrow=c(3,1))
acf(resid(udMod)) 
acf(resid(distMod))
acf(resid(dirMod))
# partial effect plot (smooth terms)
# vis.gam(udMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
# vis.gam(distMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
# vis.gam(dirMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
plot.gam(udMod, select=1, shade=T)
plot.gam(distMod, select=1, shade=T)
plot.gam(dirMod, select=1, shade=T)


