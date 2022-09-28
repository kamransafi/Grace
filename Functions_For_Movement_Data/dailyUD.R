library('move')
library('lubridate')
library(rgeos) #gCentroid

# bursted move object by days
# calculates dbb variance on bursted move object
# calculates daily motion variance sumarized as indicated by "functionDailyMotionVariance", saved as "dailyMotionVar_.....rds"
# removes days that have less locations than "minLocationsDay", by setting @interest==F
# calculates dBB per day looping through the splited dbbvarburst object
# calculates ud size and centroid, saved as "dailyUDsize_...rds" and "dailyCentroid_....rds"
# extracts corrdinates and values from dbb, saved as "dailyDBBcoordinatesValues_...rds",   NOTE many values per day, but all in one table per indiv

files <- list.files("~/GIT_SYNC/Grace/NoPush/testIndivsForUD/buffaloes",full.names=T) #wilddogs buffaloes
## storks 1 only 5 loc/day good indiv to check stop script if not enought points
# moveObj <- readRDS(files[[10]])

# pathToMV
rasterLayer <- 1000 ## resolution in mts of raster
locationError <- 20 ## location error
extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
pathToOutputFolder <- "~/GIT_SYNC/Grace/NoPush/"
minLocationsDay <- 10 # min locations required per day
functionDailyMotionVariance <- "mean" # how to sumarize the daily variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted

lapply(files[1:2],function(x){ #x is pathToMV
  tryCatch(dailydBBud(x,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage),error=function(e) NULL) # try catch because if individual contains 0 days with enough locations, I build in a stop
  })



dailydBBud <-  function(pathToMV,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage){
moveObj <- readRDS(pathToMV)
roundTS <- floor_date(timestamps(moveObj), "day") 
locPerDayDF <- data.frame(table(roundTS))
locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
daysToInclude <- locPerDayDF$roundTS[locPerDayDF$Freq>=minLocationsDay]
if(length(daysToInclude)==0){stop(paste("individual: ",  moveObj@idData$individual.local.identifier,"contains 0 days with min nb of locations"))}#jump this individual with warning message? saving it in a table?...
dailyBurst <- burst(moveObj,f=as.character(roundTS[-length(roundTS)]))
dailyBurst_c <- spTransform(dailyBurst,center=T)
dailydBBvar <- brownian.motion.variance.dyn(dailyBurst_c, location.error=locationError,margin=11, window.size=31)

###########################
## daily motion variance ## 
###########################
motionVar <- data.frame(date= floor_date(timestamps(moveObj), "day"), motVar = getMotionVariance(dailydBBvar))
aggMotionVar <- aggregate(motionVar$motVar, by=list(motionVar$date), FUN=functionDailyMotionVariance)

dailyMotionVar <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$individual.local.identifier, aggMotionVar$Group.1, sep="_"),
                             # date=aggMotionVar$Group.1,
                             motionVariance=aggMotionVar$x,
                             aggregationMotionVariance=functionDailyMotionVariance, ## included this column for sanity check, just to make sure we know what we are doing without having to go back to the code
                             row.names = NULL)
indiv <- moveObj@idData$individual.local.identifier
if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
saveRDS(dailyMotionVar, file=paste0(pathToOutputFolder,"dailyMotionVar_",moveObj@idData$study.id,"_",indiv,".rds"))


#####################################################################
## selecting days with minimum nb of locations (=minLocationsDay)  ##
#####################################################################
dailydBBvar@interest[!floor_date(timestamps(dailydBBvar), "day") %in% daysToInclude] <- FALSE # calc UD only on days with enough points
dailydBBvarL <- split(dailydBBvar)
## removing days with all @interest==FALSE
dailydBBvarL_sel <- lapply(dailydBBvarL, function(x){if(any(x@interest)){x}else{x <- NULL}})
dailydBBvarL_sel <- dailydBBvarL_sel[which(!sapply(dailydBBvarL_sel, is.null))] # removing "NULL" elements from list

#################
## daily dBBMM ##
################
dailydBBL <- lapply(dailydBBvarL_sel, function(dBBvar){
  db_r <- raster(ext=extent(dBBvar)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=1000, crs=projection(dBBvar),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
  brownian.bridge.dyn(dBBvar, raster=db_r,location.error=rep(20,length(dBBvar)),margin=11, window.size=31)
})

##############
## daily UD ##
##############
dailyUDL <-  lapply(dailydBBL, function(dBB){  
  getVolumeUD(dBB)
})

###################
## daily UD size ##
###################
UDsizeL <- lapply(dailyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  UDsizeKm2 <- cellStats(UDsel, 'sum')
  return(UDsizeKm2)
})
dateschr <- names(dailyUDL)
dateschr <- gsub("X","",dateschr)
dates <- as.Date(dateschr,format="%Y.%m.%d",tz="UTC")

dailyUDsize <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$individual.local.identifier, dates, sep="_"),
                          # date=dates,
                          UDsizeKm2=unlist(UDsizeL),
                          row.names = NULL)

indiv <- moveObj@idData$individual.local.identifier
if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
saveRDS(dailyUDsize, file=paste0(pathToOutputFolder,"dailyUDsize_",moveObj@idData$study.id,"_",indiv,".rds"))

#######################
## daily UD centroid ##
#######################
# ud <- dailyUDL[[100]]
centroidsL <- lapply(dailyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  ud_spdf <- as(UDsel,"SpatialPointsDataFrame")
  centr <- gCentroid(ud_spdf)
  centr_ll <- spTransform(centr,projection(moveObj))
  centr_coor <- coordinates(centr_ll)
  return(centr_coor)
})
centroids <- do.call("rbind",centroidsL)
dailyCentroid <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$individual.local.identifier, dates, sep="_"),
                             # date=dates,
                             longitude=centroids[,1],
                             latitude=centroids[,2],
                             row.names = NULL)

indiv <- moveObj@idData$individual.local.identifier
if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
saveRDS(dailyCentroid, file=paste0(pathToOutputFolder,"dailyCentroid_",moveObj@idData$study.id,"_",indiv,".rds"))

######################################
## daily dBB coordinates and values ##
######################################
dBBcoordinatesL <- lapply(1:length(dailydBBL), function(x){ 
  ud <- dailyUDL[[x]]
  dbb <- dailydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
  dbb_df <- as.data.frame(dbb_spdf_ll)
  dbb_df$datechr <- names(dailydBBL[x])
  return(dbb_df)
})

dBBcoordinates <- do.call("rbind",dBBcoordinatesL)
dbb_dateschr <- gsub("X","",dBBcoordinates$datechr)
dbb_dates <- as.Date(dbb_dateschr,format="%Y.%m.%d",tz="UTC")


dailyDBBcoordinatesValues <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$individual.local.identifier, dbb_dates, sep="_"),
                             date=dbb_dates,
                             longitude=dBBcoordinates$x,
                             latitude=dBBcoordinates$y,
                             dBBvalues=dBBcoordinates$layer,
                             row.names = NULL)

indiv <- moveObj@idData$individual.local.identifier
if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
saveRDS(dailyDBBcoordinatesValues, file=paste0(pathToOutputFolder,"dailyDBBcoordinatesValues_",moveObj@idData$study.id,"_",indiv,".rds"))
}



## this is how one do the raster to spdf in sf, but it probably does not make sense in this case as 
# library(stars)
# dbbSel_t_stars <- st_as_stars(dbbSel_t)
# library(sf)
# st_as_sf(dbbSel_t_stars, as_points = TRUE, merge = FALSE)






