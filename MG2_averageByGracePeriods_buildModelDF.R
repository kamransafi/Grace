
# Bring in the table with the calculated metrics per day, merge the information and average them by the grace collection periods
# Use this to build a dataset and run the following models:
#_______________
# 1- monthly average of daily UD size ~ monthly grace experienced (calculated by multiplying UD probabilities * monthly grace values) - we expect larger UDs in poorer conditions (negative grace) (neg. corr)
# 2- monthly average of daily cumulative distance ~ monthly grace experienced (calculated as above) - higher daily distance in poorer conditions (negative grace) (neg. corr)
# 3- monthly "directness" of movement ~ monthly grace experienced (calculated as above) - we expect more directed movement (lower dbb variance) in poorer conditions (negative grace) (pos. corr)
#_______________

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

library(lubridate)

#___________________________________
# CREATE A DATAFRAME FOR THE MODELS

# Import table with metrics and grace experienced per day and per individual
daily_graceExp <- readRDS("MovementData/MovementMetrics/dailyGraceExperienced_allInds_1km.rds")
# and the table including individual infos, locomotory mode, daily movement metrics and ud size
daily_metrics <- readRDS("MovementData/MovementMetrics/daily_movementMetrics_allInds_200m.rds")

# merge the two daily tables
#table(daily_graceExp$commonID %in% daily_metrics$commonID)
dailyDF <- merge(daily_metrics, daily_graceExp[,-2], by="commonID", all.y=T)
# Create two variables for aggregation, one pasting MBid_individual and one pasting start and end of the grace collection period
#MBid_indiv <- sapply(lapply(strsplit(daily_graceExp$commonID,"_"),"[",1:2),paste,collapse="_")
MBid_indiv <- paste(dailyDF$MBid, dailyDF$individual, sep="_")
graceCollPeriod <- paste(dailyDF$start_graceCollection, dailyDF$end_graceCollection, sep="_")

# Average variables by collection period (more or less monthly)
#test <- aggregate(cbind(UDsizeKm2,graceExperienced) ~ MBid_indiv + graceCollPeriod, data=dailyDF, FUN=mean)
modelDF <- aggregate(cbind(UDsizeKm2,cumulativeDist_km,motionVariance,graceExperienced,UDwMeanLongitude,UDwMeanLatitude,locsPerDay) 
                     ~ MBid_indiv + graceCollPeriod + species + Locomotion + grace_layer + grace_seq,
                     data=dailyDF, FUN=mean)
modelDF$grace_seq <- as.numeric(modelDF$grace_seq)
modelDF$grace_layer <- as.Date(modelDF$grace_layer)

# Some checks
length(unique(modelDF$MBid_indiv)) # number of individuals included in the model (non only NA values associated to grace)
table(modelDF$Locomotion) # n. of days per locomotion type
table(table(modelDF$MBid_indiv)>1) #how many observations/months per individual
head(sort(table(modelDF$MBid_indiv), decreasing=T))

#___________________
## explore response variables

# temporal pattern in grace (grace sequence goes from 1 to the nlayers)
test <- aggregate(modelDF$graceExperienced~modelDF$grace_seq, FUN=mean)
par(mfrow=c(1,1))
plot(test[,2]~test[,1], type="b",
     xlab="Grace sequence", ylab="Grace")
# unrealistically high UD sizes
udQuant <- quantile(modelDF$UDsizeKm2, seq(0,1,0.001))
plot(udQuant)
plot(udQuant[udQuant < udQuant["99.4%"]])
modelDF[modelDF$UDsizeKm2 > 200,]
summary(modelDF$UDsizeKm2[modelDF$species == "Felis catus"])
summary(modelDF$UDsizeKm2[modelDF$species == "Alces alces"])
summary(modelDF$UDsizeKm2[modelDF$species == "Bos taurus"])
summary(modelDF$UDsizeKm2[modelDF$species == "Lycaon pictus"])
summary(modelDF$UDsizeKm2[modelDF$species == "Vulpes lagopus"])
summary(modelDF$UDsizeKm2[modelDF$species == "Canis lupus"])
# large cumulative distances correspond to large UD sizes, so the filter will work on both
distQuant <- quantile(modelDF$cumulativeDist_km, seq(0,1,0.01))
plot(distQuant, ylim=c(0,100))
modelDF[modelDF$cumulativeDist_km > 30,]
# very large motion variance (directness) also correspond to very large UD sizes
dirQuant <- quantile(modelDF$motionVariance, seq(0,1,0.001))
plot(dirQuant, ylim=c(0,40000))
modelDF[modelDF$motionVariance > 35000,]

# remove all observations with UD sized > 99.4% == 769.8 Km2 (only 24 observations)
modelDF_sub <- modelDF[which(modelDF$UDsizeKm2 < udQuant["99.4%"]),]

# For now remove swimming species (one species of seal) and correct error in locomotion of bos genus (walking not flying)
# unique(modelDF_sub$species[modelDF_sub$Locomotion=="swimming"])
# unique(modelDF_sub$species[modelDF_sub$Locomotion=="flying"])
modelDF_sub <- modelDF_sub[which(modelDF_sub$Locomotion!="swimming"),]
#8241619, 172506851, 896410936, 1233598831 Bos studies (cattle, should they be included?)

# Calculate UD size differences from a species average UD size
library(data.table)
sp_ls <- split(modelDF_sub, modelDF_sub$species)
modelDF_sub <- as.data.frame(rbindlist(lapply(sp_ls, function(sp){
  sp$UD_speciesAvg <- mean(sp$UDsizeKm2, na.rm=T)
  sp$nUds_perSpAvg <- nrow(sp)
  sp$nInds_perSpAvg <- length(unique(sp$MBid_indiv))
  sp$diff_fromAvgUd <- sp$UDsizeKm2 - mean(sp$UDsizeKm2)
  sp$diff_fromAvgUd_norm <- (sp$diff_fromAvgUd - min(sp$diff_fromAvgUd))/(max(sp$diff_fromAvgUd)-min(sp$diff_fromAvgUd))
  sp$diff_fromAvgUd_scale <- as.numeric(scale(sp$diff_fromAvgUd)) #(x - mean(x)) / sd(x)
  sp$diff_fromAvgUd_perc <- (sp$diff_fromAvgUd/sp$UD_speciesAvg)*100
  return(sp)
})))

par(mfrow=c(2,2))
hist(modelDF_sub$UDsizeKm2, breaks="FD")
hist(log(modelDF_sub$UDsizeKm2), breaks="FD")
hist(modelDF_sub$diff_fromAvgUd, breaks="FD")
hist(modelDF_sub$diff_fromAvgUd_scale, breaks="FD")
hist(modelDF_sub$diff_fromAvgUd_perc, breaks="FD")

saveRDS(modelDF_sub, file="MovementData/MovementMetrics/modelDataset.rds")

#________
# MODELS
#________

library(mgcv)

modelDF_sub <- readRDS("MovementData/MovementMetrics/modelDataset.rds")

modelDF_sub$species <- as.factor(modelDF_sub$species) # needed for random effect
modelDF_sub$MBid_indiv <- as.factor(modelDF_sub$MBid_indiv)
modelDF_sub$Locomotion <- as.factor(modelDF_sub$Locomotion)

graceQuant <- quantile(modelDF_sub$graceExperienced, seq(0,1,0.001))
plot(graceQuant)
hist(modelDF_sub$graceExperienced, breaks="FD")
plot(log(UDsizeKm2)~locsPerDay, data=modelDF_sub)

modelDF_sub <- modelDF_sub[modelDF_sub$graceExperienced >= graceQuant["0.5%"] &
                             modelDF_sub$graceExperienced <= graceQuant["99.9%"],]

#___________________
## modelling UD size

# month_grace_layer <- month(modelDF_sub$grace_layer)
# year_grace_layer <- year(modelDF_sub$grace_layer)

# boxcox transformation did not improve model residuals so we exclude it
# library(forecast)
# lambda <- BoxCox.lambda(modelDF_sub$UDsizeKm2)
# UDsize_boxcox <- BoxCox(modelDF_sub$UDsizeKm2, lambda)
# hist(UDsize_boxcox, breaks="FD")

udMod_spec_walk <- gam(log(UDsizeKm2) ~
                         s(graceExperienced) +
                         s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                         grace_seq + #(time, since first measurement of Grace)
                         UD_speciesAvg +
                         locsPerDay + #to account for UD size varying depending on number of daily locations
                         s(species, bs="re"), #s(species, bs="re"), s(MBid_indiv, bs="re")#(random factors)
                       data=modelDF_sub[which(modelDF_sub$Locomotion=="walking"),],
                       na.action=na.fail)
udMod_spec_fly <- gam(log(UDsizeKm2) ~
                        s(graceExperienced) +
                        s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                        grace_seq + #(time, since first measurement of Grace)
                        UD_speciesAvg +
                        locsPerDay + #to account for UD size varying depending on number of daily locations
                        s(species, bs="re"), #s(species, bs="re"), s(MBid_indiv, bs="re")#(random factors)
                      data=modelDF_sub[which(modelDF_sub$Locomotion=="flying"),],
                      na.action=na.fail)
plot.gam(udMod_spec_walk, select=1, shade=T)
plot.gam(udMod_spec_fly, select=1, shade=T)
par(mfrow=c(2,2))
gam.check(udMod_spec_walk)
gam.check(udMod_spec_fly)


udMod_spec <- gam(log(UDsizeKm2) ~
                    s(graceExperienced) +
                    s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                    grace_seq + #(time, since first measurement of Grace)
                    #s(month_grace_layer, bs = "cc") + s(year_grace_layer) +
                    #species +
                    UD_speciesAvg +
                    locsPerDay + #to account for UD size varying depending on number of daily locations
                    Locomotion + #(locomotory.mode)
                    s(species, bs="re"), #s(species, bs="re"), s(MBid_indiv, bs="re")#(random factors)
                  #correlation=corAR1(form=~1|grace_layer),
                  data=modelDF_sub,
                  na.action=na.fail) #by default
udMod_spec$sp
udMod_ind$sp
summary(udMod_spec)
summary(udMod_ind)
acf(residuals(udMod_spec))
acf(residuals(udMod_ind))
plot.gam(udMod_spec, select=1, shade=T)
plot.gam(udMod_ind, select=1, shade=T)
par(mfrow=c(2,2))
gam.check(udMod_spec)
gam.check(udMod_ind)

# library("DHARMa")
# check_model <- simulateResiduals(fittedModel = udMod, n = 500)
# plot(check_model)

udMod_ind_null <- gam(log(UDsizeKm2) ~
                        s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) +
                        grace_seq +
                        UD_speciesAvg +
                        locsPerDay +
                        #Locomotion +
                        s(MBid_indiv, bs="re"),
                      data=modelDF_sub,
                      na.action=na.fail)
par(mfrow=c(2,2));gam.check(udMod_null)
summary(udMod_ind)$dev.expl;summary(udMod_null)$dev.expl
AIC(udMod_ind);AIC(udMod_null)

save(udMod_spec, udMod_ind, udMod_ind_null, file="ModelsResults/models_UDsize.rdata")

#____________________________________
## modelling perc. change in UD size

sort(table(modelDF_sub$nUds_perSpAvg), decreasing=T)
sort(table(modelDF_sub$nInds_perSpAvg), decreasing=T)
plot(modelDF_sub$diff_fromAvgUd_perc~modelDF_sub$nUds_perSpAvg, xlim=c(1,50))

modelDF_subsub <- modelDF_sub[modelDF_sub$nUds_perSpAvg > 10,]
percQuant <- quantile(modelDF_subsub$diff_fromAvgUd_perc, seq(0,1,0.001))
plot(percQuant)
modelDF_subsub <- modelDF_subsub[which(modelDF_subsub$diff_fromAvgUd_perc < percQuant["99.8%"]),]
hist(scale(modelDF_subsub$diff_fromAvgUd_perc, breaks="FD"))
hist(modelDF_subsub$diff_fromAvgUd_scale, breaks="FD")


change_udMod_spec <- gam(diff_fromAvgUd_perc ~
                          s(graceExperienced) +
                          s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                          grace_seq +
                          locsPerDay + #to account for UD size varying depending on number of daily locations
                          nUds_perSpAvg + #to account for the fact that differences are likely to be bigger if the mean was calculated from more individuals
                          #Locomotion + #(locomotory.mode)
                          s(species, bs="re"), #s(species, bs="re"), s(MBid_indiv, bs="re") #(random fac)
                        data=modelDF_subsub,#[-991,],
                        #method="GCV.Cp" #default, otherwise method="REML"
                        na.action=na.fail) #by default
change_udMod_spec$sp
summary(change_udMod_spec)
acf(residuals(change_udMod_spec))
acf(residuals(change_udMod_ind))
plot.gam(change_udMod_spec, select=1, shade=T)
plot.gam(change_udMod_ind, select=1, shade=T)
par(mfrow=c(2,2))
gam.check(change_udMod_spec)
gam.check(change_udMod_ind)

modelDF_subsub[which(fitted(change_udMod)>200),] #one "outlier"

save(change_udMod_spec, change_udMod_ind, file="ModelsResults/models_UDpercChange.rdata")

#_______________________________
## modelling cumulative distance

hist(modelDF_sub$cumulativeDist_km, breaks="FD")
distQuant <- quantile(modelDF_sub$cumulativeDist_km, seq(0,1,0.001))
plot(distQuant)
distQuant
modelDF_subsub <- modelDF_sub[modelDF_sub$cumulativeDist_km <= distQuant["99.8%"],]


distMod_walk <- gam(log(cumulativeDist_km) ~
                 s(graceExperienced) +
                 s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                 grace_seq + #(time, since first measurement of Grace)
                 locsPerDay + #to account for UD size varying depending on number of daily locations
                 s(species, bs="re"), #(random fac)
               data=modelDF_subsub[which(modelDF_subsub$Locomotion=="walking"),],
               na.action=na.fail)
summary(distMod_walk)
hist(residuals(distMod_walk))
plot.gam(distMod_walk, select=1, shade=T)

distMod_fly <- gam(log(cumulativeDist_km) ~
                      s(graceExperienced) +
                      s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                      grace_seq + #(time, since first measurement of Grace)
                      locsPerDay + #to account for UD size varying depending on number of daily locations
                      s(species, bs="re"), #(random fac)
                    data=modelDF_subsub[which(modelDF_subsub$Locomotion=="flying"),],
                    na.action=na.fail)
summary(distMod_fly)
hist(residuals(distMod_fly))
plot.gam(distMod_fly, select=1, shade=T)


#________
## modelling directness of movement

hist(modelDF_sub$motionVariance, breaks="FD")
varQuant <- quantile(modelDF_sub$motionVariance, seq(0,1,0.001))
plot(varQuant)
varQuant
modelDF_subsub <- modelDF_sub[modelDF_sub$motionVariance <= varQuant["99.3%"],]

dirMod_walk <- gam(log(motionVariance) ~
                    s(graceExperienced) +
                    s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                    grace_seq + #(time, since first measurement of Grace)
                    locsPerDay + #to account for UD size varying depending on number of daily locations
                    s(species, bs="re"), #(random fac)
                  data=modelDF_subsub[which(modelDF_subsub$Locomotion=="walking"),],
                  na.action=na.fail)
summary(dirMod_walk)
plot.gam(dirMod_walk, select=1, shade=T)
par(mfrow=c(2,2))
gam.check(dirMod_walk)


dirMod_fly <- gam(log(motionVariance) ~
                s(graceExperienced) +
                s(UDwMeanLongitude,UDwMeanLatitude, by=grace_seq) + #(weighted coordinates from dailyUD)
                grace_seq + #(time, since first measurement of Grace)
                locsPerDay + #to account for UD size varying depending on number of daily locations
                s(species, bs="re"), #(random fac)
              data=modelDF_subsub[which(modelDF_subsub$Locomotion=="flying"),],
              na.action=na.fail)
summary(dirMod_fly)
plot.gam(dirMod_fly, select=1, shade=T)
par(mfrow=c(2,2))
gam.check(dirMod_fly)


#__________________
# diagnostic plots
x11();par(mfrow=c(2,2))
gam.check(udMod) 
gam.check(distMod) 
gam.check(dirMod) 
# temporal autocorrelation
par(mfrow=c(3,2))
acf(resid(udMod));acf(resid(udMod_null))
acf(resid(distMod));acf(resid(distMod_null))
acf(resid(dirMod));acf(resid(dirMod_null))
# partial effect plot (smooth terms)
# vis.gam(udMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
# vis.gam(distMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
# vis.gam(dirMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
#plot.gam(dirMod, pages=1, shade=T)
layout(matrix(c(1,2,3,3), byrow=T, nrow=2))
plot.gam(udMod, select=1, shade=T)
plot.gam(distMod, select=1, shade=T)
plot.gam(dirMod, select=1, shade=T)

#_____________________________________
# Predict UD model on the same dataset

library(raster)
library(RColorBrewer)
library(viridis)

# Import grace layers
atlGrace_infoBr <- readRDS("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.rds")

# Extract fitted values of the UD model
modelDF$fittedUD <- exp(predict(udMod))
summary(modelDF$fittedUD)
summary(modelDF$UDsizeKm2)

# Rasterize the fitted values for some species
coordinates(modelDF) <- ~ UDwMeanLongitude + UDwMeanLatitude
proj4string(modelDF) <- "+proj=longlat +ellps=WGS84 +no_defs"

modelDF_sp <- split(modelDF, modelDF$species)
sort(sapply(modelDF_sp, nrow), decreasing = T)

sp <- modelDF_sp[["Cervus elaphus"]]

udStack_sp <- stack(lapply(modelDF_sp, function(sp){
  predR <- rasterize(sp, atlGrace_infoBr, field="fittedUD", fun=mean)
  return(predR)
}))
cuts <- round(seq(min(modelDF$fittedUD), max(modelDF$fittedUD), 20))
pal <- colorRampPalette(c("dodgerblue","orange","yellow"))
plot(extent(udStack_sp), bty="n", axes=F, type="n", xlab="",ylab="")
plot(udStack_sp, legend.only=TRUE, col=pal(7),
     legend.width=1, legend.shrink=0.75,
     smallplot=c(0.75,0.8, 0.2,1),
     axis.args=list(at=cuts, labels=cuts, cex.axis=1),
     legend.args=list(text='UD size (km2)', side=3, font=1, line=1.2, cex=0.75))
par(mar = par("mar"))
lapply(1:nlayers(udStack_sp), function(i){
  plot(udStack_sp[[i]], add=T, legend=F,
       breaks=cuts, col=pal(7))
})

#_____________________________________
# Project UD model on one grace layer

library(raster)
library(rasterVis)
library(viridis)

# Import grace layers
atlGrace_infoBr <- readRDS("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.rds")
atlGrace_br <- brick("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.tif")
atlGrace_br@z <- atlGrace_infoBr@z

# Choose on which layer to do the prediction
table(year(modelDF$grace_layer), month(modelDF$grace_layer))
grace4proj <- atlGrace_br[[which(atlGrace_br@z$Date=="2009-04-16")]]
levelplot(grace4proj)

# Create new dataframe for prediction
projDf <- as.data.frame(rasterToPoints(grace4proj, spatial=F))
names(projDf) <- c("UDwMeanLongitude","UDwMeanLatitude","graceExperienced")
projDf$month_grace_layer <- month(grace4proj@z[[1]])
projDf$year_grace_layer <- year(grace4proj@z[[1]])
projDf$locsPerDay <- 24#mean(modelDF$locsPerDay)
projDf$mergeCol <- 1:nrow(projDf)
spec <- expand.grid(mergeCol=projDf$mergeCol, species=unique(modelDF$species))

projDf <- merge(projDf, spec, by="mergeCol", all.y=T)

# Limit prediction dataset to the range of coordinates and the range of grace used to train the model
projDf <- projDf[projDf$UDwMeanLongitude >= min(modelDF$UDwMeanLongitude) & projDf$UDwMeanLongitude <= max(modelDF$UDwMeanLongitude) &
                   projDf$UDwMeanLatitude >= min(modelDF$UDwMeanLatitude) & projDf$UDwMeanLatitude <= max(modelDF$UDwMeanLatitude),]
projDf <- projDf[projDf$graceExperienced >= min(modelDF$graceExperienced) &
                   projDf$graceExperienced <= max(modelDF$graceExperienced),]

# Predict UD response variable under the above conditions
projDf$UDsizeKm2 <- exp(predict(udMod, newdata=projDf))
boxplot(UDsizeKm2~species, data=projDf, ylim=c(0,500))
boxplot(UDsizeKm2~species, data=modelDF, ylim=c(0,300))
summary(modelDF$UDsizeKm2)
summary(projDf$UDsizeKm2)

# Map predictions, one layer per species
coordinates(projDf) <- ~ UDwMeanLongitude + UDwMeanLatitude
proj4string(projDf) <- "+proj=longlat +ellps=WGS84 +no_defs"

projDf_sp <- split(projDf, projDf$species)
predStack_sp <- stack(lapply(projDf_sp, function(sp){
  predR <- rasterize(sp, grace4proj, field="UDsizeKm2", fun=mean)
  return(predR)
}))
levelplot(predStack_sp[[1:10]])
levelplot(predStack_sp[[1]])
plot(stretch(predStack_sp[[1]], maxq=.95), col=viridis(10), legend=F, alpha=.65)


