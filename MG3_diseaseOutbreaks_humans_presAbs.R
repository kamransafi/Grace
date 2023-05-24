setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

# Import list of GRACE stacks
atlGrace_infoBr <- readRDS("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.rds")
atlGrace_br <- brick("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.tif")
names(atlGrace_br) <- names(atlGrace_infoBr)
atlGrace_br@z <- atlGrace_infoBr@z
# Import table with the GRACE collection periods
graceTimes <- readRDS("RemoteSensingData/grace_tws_collectionPeriods.rds")
graceTimes <- graceTimes[order(graceTimes$grace_time),]
# Import table of disease outbreaks sent by Louis and make it spatial
outb <- read.csv("DiseaseOutbreaks_Louis/FAO-EMPRES.csv")
outb$observation_date <- as.Date(outb$observation_date, "%Y-%m-%d")
outb <- outb[order(outb$observation_date),]

# subset the outbreaks dataset to only the time range in which GRACE was collected
min(graceTimes$start_collection); max(graceTimes$end_collection)
outb <- outb[outb$observation_date >= min(graceTimes$start_collection) & 
               outb$observation_date <= max(graceTimes$end_collection),]
range(outb$observation_date)

# subset only outbreak events that affected humans and that are confirmed
outbH <- outb[which(outb$humans_affected > 0 & outb$diagnosis_status == "Confirmed"),]

#___________________________
## Create background points

# create for each outbreak event (occurrence=1) some "background points", at the same locations, in different years
outbH$occurrence <- 1
outbH$event_id_new <- outbH$event_id
graceYears <- unique(year(graceTimes$grace_time))

outb_background <- as.data.frame(rbindlist(lapply(1:nrow(outbH), function(i){
  abs <- outbH[rep(i, length(graceYears)-1),]
  abs$occurrence <- 0
  abs$event_id_new <- paste(abs$event_id, 1:nrow(abs), sep="_")
  # set alternative dates by changing the year
  my <- format(abs$observation_date[1], "-%m-%d")
  if(my == "-02-29"){my <- "-02-28"} # to avoid problems with leap years
  alternativeDates <- as.Date(paste0(graceYears, my), "%Y-%m-%d")
  abs$observation_date <- alternativeDates[year(alternativeDates) != year(abs$observation_date[1])]
  # set to NA the other columns
  abs[,c("report_date","display_date", "diagnosis_status","humans_affected","humans_deaths","diagnosis_source")] <- NA
  return(abs)
})))
length(unique(outb_background$event_id))==nrow(outbH)

# rbind presence and absence datasets and remove again observations outside the grace periods
outbH_pa <- rbind(outbH, outb_background)
outbH_pa <- outbH_pa[order(outbH_pa$event_id, outbH_pa$observation_date),]
outbH_pa <- outbH_pa[outbH_pa$observation_date >= min(graceTimes$start_collection) & 
                       outbH_pa$observation_date <= max(graceTimes$end_collection),]

#__________________
## Associate grace

# Transform in spatial object
coordinates(outbH_pa) <- c("lon","lat")
proj4string(outbH_pa) <- "+proj=longlat +ellps=WGS84 +no_defs"

# associate to each row the corresponding grace layer and collection period
outbH_pa@data[,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.character(NA)
# for days within the collection period difference in time = 0, 
# for days outside the collection period we will measure time difference in days
outbH_pa$timeDiff_daysFromCollection <- 0

for(i in 1:nrow(outbH_pa)){
  print(i)
  minTimeDiff <- which.min(abs(difftime(outbH_pa$observation_date[i], graceTimes$grace_time, units="days")))
  closerGrace <- graceTimes[minTimeDiff,]
  # if the outbreak observation date is not comprised in the collection period of the closest grace layer
  # a time difference in days from the start/end of the closest collection period is included in the dataframe
  if(outbH_pa$observation_date[i] < closerGrace$start_collection | 
     outbH_pa$observation_date[i] > closerGrace$end_collection){
    outbH_pa$timeDiff_daysFromCollection[i] <- min(abs(difftime(outbH_pa$observation_date[i], 
                                                            c(closerGrace$start_collection,closerGrace$end_collection), units="days")))
  }
  # otherwise the time difference stays as 0.
  # This way all observation dates are always associated to a grace layer (even if farther away in time) 
  outbH_pa[i,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.matrix(closerGrace[,c("grace_time","start_collection","end_collection")])
}
outbH_pa@data$start_graceCollection <- as.Date(outbH_pa@data$start_graceCollection,"%Y-%m-%d")
outbH_pa@data$end_graceCollection <- as.Date(outbH_pa@data$end_graceCollection,"%Y-%m-%d")
# there should be no NAs associated to the observation dates within the grace collection period
table(is.na(outbH_pa@data$grace_layer))
# check what is the maximum time difference from the closest grace layer
summary(outbH_pa$timeDiff_daysFromCollection)
outbH_pa@data[outbH_pa$timeDiff_daysFromCollection>100,]

# For the next step we need the raster brick with all the GRACE layers, we will extract values from the layer corresponding to each observation date
# outbreak observations in days that could not be associated to any grace layer are automatically excluded from the output dataframe
outb_graceLs <- split(outbH_pa, outbH_pa$grace_layer)

outb_grace_pa <- as.data.frame(rbindlist(lapply(outb_graceLs, function(outb_gr){
  graceLayer <- atlGrace_br[[which(atlGrace_br@z$Date == unique(outb_gr$grace_layer))]]
  outb_gr$grace_tws <- extract(graceLayer, outb_gr, method="bilinear") 
  return(as.data.frame(outb_gr))
})))
table(is.na(outb_grace_pa$grace_tws))
summary(outb_grace_pa$grace_tws)
outb_grace_pa <- outb_grace_pa[order(outb_grace_pa$observation_date),]
outb_grace_pa$grace_layer <- as.Date(outb_grace_pa$grace_layer)

saveRDS(outb_grace_pa, file="DiseaseOutbreaks_Louis/outbreaksDF_humans_backgroundPoints_grace.rds")

#_____________________
# Some basic analysis

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")
outb_grace_pa <- readRDS("DiseaseOutbreaks_Louis/outbreaksDF_humans_backgroundPoints_grace.rds")

library(ggplot2)
ggplot(outb_grace_pa, aes(x=disease, y=grace_tws, fill=as.character(occurrence))) +
  geom_boxplot() +
  theme_bw()

#___________________________________
# modelling occurrence of outbreaks

library(mgcv)
library(lubridate)

outb_grace_pa$disease <- as.factor(outb_grace_pa$disease)
month_grace_layer <- month(outb_grace_pa$grace_layer)
year_grace_layer <- year(outb_grace_pa$grace_layer)

outMod <- gam(occurrence ~
                #s(grace_tws, by=disease) +
                #disease +
                grace_tws * disease +
                s(lon,lat) + 
                s(month_grace_layer, bs = "cc") + s(year_grace_layer) +
                s(event_id, bs="re"),
              family=binomial(), 
              data=outb_grace_pa)
summary(outMod)
hist(residuals(outMod))

outMod_null <- gam(occurrence ~
                     s(lon,lat) + 
                     s(month_grace_layer, bs = "cc") + s(year_grace_layer) +
                     s(event_id, bs="re"),
                   family=binomial(), 
                   data=outb_grace_pa)
summary(outMod)$dev.expl;summary(outMod_null)$dev.expl
AIC(outMod);AIC(outMod_null)

infMod <- gam(humans_affected ~
                #s(grace_tws, by=disease) +
                #disease +
                grace_tws * disease +
                s(lon,lat) + 
                s(month(grace_layer), bs = "cc") + s(year(grace_layer)) +
                s(event_id, bs="re"),
              family=poisson(),
              data=outb_grace_pa[which(outb_grace_pa$occurrence==1),])
summary(infMod)

#__________________
# diagnostic plots
par(mfrow=c(2,2))
gam.check(outMod) 
gam.check(infMod) 
# temporal autocorrelation ?
par(mfrow=c(1,1))
acf(resid(outMod)) 
# partial effect plot (smooth terms)
# vis.gam(udMod,view=c("graceExperienced","grace_seq"), type="response",plot.type="contour")
plot.gam(outMod, pages=1, shade=T)
plot.gam(outMod, select=1, shade=T)


