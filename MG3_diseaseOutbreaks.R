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
coordinates(outb) <- c("lon","lat")
proj4string(outb) <- "+proj=longlat +ellps=WGS84 +no_defs"
outb@data$observation_date <- as.Date(outb@data$observation_date, "%Y-%m-%d")
outb <- outb[order(outb@data$observation_date),]

# subset the outbreaks dataset to only the time range in which GRACE was collected
min(graceTimes$start_collection); max(graceTimes$end_collection)
outb <- outb[outb@data$observation_date >= min(graceTimes$start_collection) & 
               outb@data$observation_date <= max(graceTimes$end_collection),]
range(outb@data$observation_date)

# associate to each row the corresponding grace layer and collection period
outb@data[,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.character(NA)
# for days within the collection period difference in time = 0, 
# for days outside the collection period we will measure time difference in days
outb$timeDiff_daysFromCollection <- 0

for(i in 1:nrow(outb)){
  print(i)
  minTimeDiff <- which.min(abs(difftime(outb$observation_date[i], graceTimes$grace_time, units="days")))
  closerGrace <- graceTimes[minTimeDiff,]
  # if the outbreak observation date is not comprised in the collection period of the closest grace layer
  # a time difference in days from the start/end of the closest collection period is included in the dataframe
  if(outb$observation_date[i] < closerGrace$start_collection | 
     outb$observation_date[i] > closerGrace$end_collection){
    outb$timeDiff_daysFromCollection[i] <- min(abs(difftime(outb$observation_date[i], 
                                                        c(closerGrace$start_collection,closerGrace$end_collection), units="days")))
  }
  # otherwise the time difference stays as 0.
  # This way all observation dates are always associated to a grace layer (even if farther away in time) 
    outb[i,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.matrix(closerGrace[,c("grace_time","start_collection","end_collection")])
}
outb@data$start_graceCollection <- as.Date(outb@data$start_graceCollection,"%Y-%m-%d")
outb@data$end_graceCollection <- as.Date(outb@data$end_graceCollection,"%Y-%m-%d")
# there should be no NAs associated to the observation dates within the grace collection period
table(is.na(outb@data$grace_layer))
# check what is the maximum time difference from the closest grace layer
summary(outb$timeDiff_daysFromCollection)
outb@data[outb$timeDiff_daysFromCollection>100,]

# For the next step we need the raster brick with all the GRACE layers, we will extract values from the layer corresponding to each observation date
# outbreak observations in days that could not be associated to any grace layer are automatically excluded from the output dataframe
outb_graceLs <- split(outb, outb$grace_layer)

outb_grace <- as.data.frame(rbindlist(lapply(outb_graceLs, function(outb_gr){
  graceLayer <- atlGrace_br[[which(atlGrace_br@z$Date == unique(outb_gr$grace_layer))]]
  outb_gr$grace_tws <- extract(graceLayer, outb_gr, method="bilinear") 
  return(as.data.frame(outb_gr))
})))
table(is.na(outb_grace$grace_tws))
summary(outb_grace$grace_tws)

saveRDS(outb_grace, file="DiseaseOutbreaks_Louis/outbreaksDF_grace.rds")


#__________________
## Old loop script, in which days that did not fall within any of the collection period would get excluded
# for(i in 1:nrow(outb)){
#   print(i)
#   minTimeDiff <- which.min(abs(difftime(outb$observation_date[i], graceTimes$grace_time, units="days")))
#   closerGrace <- graceTimes[c(minTimeDiff-1,minTimeDiff,minTimeDiff+1),]
#   whichCollection <- outb$observation_date[i] >= closerGrace$start_collection & 
#     outb$observation_date[i] <= closerGrace$end_collection
#   # if the outbreak observation date is close to a layer but it's not comprised in the collection period the step is skipped (no grace layer is associated to that day in the outb dataframe, grace info stay as NA)
#   # otherwise the grace collection period including the observation date will be associated to the outb dataframe
#   # should there be more than one layer to associate we select the first one (by adding [1,] to the closerGrace selection)
#   if(any(whichCollection[!is.na(whichCollection)]==T)){
#     outb[i,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.matrix(closerGrace[whichCollection,c("grace_time","start_collection","end_collection")][1,])
#   }
# }
# check that all days assign to a grace layers are actually included in the data collection period
# print(all(outb@data$observation_date >= outb@data$start_graceCollection & 
#            outb@data$observation_date <= outb@data$end_graceCollection, na.rm=T))
# check how many outbreaks don't fall in one of the grace collection period
# table(is.na(outb@data$grace_layer)) #25745 NAs
