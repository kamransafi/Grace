library('move')
library('lubridate')
library("rgeos") #gCentroid

# bursted move object by days
# calculates dbb variance on bursted move object
# calculates daily motion variance summarized as indicated by "functionDailyMotionVariance", saved as "dailyMotionVar_.....rds"
# removes days that have less locations than "minLocationsDay", by setting @interest==F
# calculates dBB per day looping through the splitted dbbvarburst object
# calculates ud size, centroid and weighted lat/long coords, saved as "dailyUDcalc_...rds" 
# extracts coordinates and values from dbb, in SPDF, saved as "dailyDBBcoordinatesSPDF_...rds"


### here to...##
files <- list.files("~/GIT_SYNC/Grace/NoPush/testIndivsForUD/storks",full.names=T) #wilddogs buffaloes
## storks 1 only 5 loc/day good indiv to check stop script if not enough points
# moveObj <- readRDS(files[[10]])

# pathToMV <- files[10]
rasterLayer <- 1000 ## resolution in mts of raster
locationError <- 20 ## location error
extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
pathToOutputFolder <- "~/GIT_SYNC/Grace/NoPush/outputTestRuns/"
minLocationsDay <- 8 # min locations required per day
functionDailyMotionVariance <- "mean" # how to sumarize the daily variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted

##################
is.error <- function(x) inherits(x, "try-error")

results <- lapply(files[1:2],function(x)try({ #x is pathToMV
  dailydBBud(x,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage) # try catch because if individual contains 0 days with enough locations, I build in a stop
}))

table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
files[1:2][vapply(results, is.error, logical(1))]

###### ....here goes into MX_UDcalcuc.R file #######################

dailydBBud <-  function(pathToMV,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage){
  # start_time <- Sys.time()
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  daysToInclude <- locPerDayDF$roundTS[locPerDayDF$Freq>=minLocationsDay]
  if(length(daysToInclude)==0){stop(paste("individual: ",  moveObj@idData$individual.local.identifier,"contains 0 days with min nb of locations"))}#jump this individual with warning message? saving it in a table?...
  dailyBurst <- burst(moveObj,f=as.character(roundTS[-length(roundTS)]))
  dailyBurst_c <- spTransform(dailyBurst,center=T)
  dailydBBvar <- brownian.motion.variance.dyn(dailyBurst_c, location.error=locationError,margin=11, window.size=31)
  
  # saveRDS(dailydBBvar, file=paste0(pathToOutputFolder,"dailydBBvar_",moveObj@idData$study.id,"_",indiv,".rds"))
  ###########################
  ## daily motion variance ## 
  ###########################
  motionVar <- data.frame(date= floor_date(timestamps(moveObj), "day"), motVar = getMotionVariance(dailydBBvar))
  aggMotionVar <- aggregate(motionVar$motVar, by=list(motionVar$date), FUN=functionDailyMotionVariance)
  
  aggMotionVarNbLoc <- merge(aggMotionVar, locPerDayDF, by.x="Group.1",by.y="roundTS", all.x=T)
  
  dailyMotionVar <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, aggMotionVar$Group.1, sep="_"),
                               individual=indiv,
                               date=aggMotionVarNbLoc$Group.1,
                               locsPerDay=aggMotionVarNbLoc$Freq,
                               motionVariance=aggMotionVarNbLoc$x,
                               aggregationMotionVariance=functionDailyMotionVariance, ## included this column for sanity check, just to make sure we know what we are doing without having to go back to the code
                               row.names = NULL)
  
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
  datesDF <- data.frame(date=as_datetime(as.character(dateschr), tz="UTC", format="%Y.%m.%d"))
  includedDatesNbLoc <- merge(datesDF, locPerDayDF, by.x="date",by.y="roundTS", all.x=T)
  
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
  
  ########################################
  ## daily dBB weighted mean coordinates ##
  ########################################
  dBBwCoordinatesL <- lapply(1:length(dailydBBL), function(x){ 
    ud <- dailyUDL[[x]]
    dbb <- dailydBBL[[x]]
    dbb[ud>UDpercentage] <- NA
    dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
    dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
    dbb_df <- as.data.frame(dbb_spdf_ll)
    wMeanLong <-  weighted.mean(dbb_df$x,dbb_df$layer) 
    wMeanLat <-  weighted.mean(dbb_df$y,dbb_df$layer) 
    wMeanLL <- data.frame(date=names(dailydBBL[x]), wMeanLong=wMeanLong, wMeanLat=wMeanLat)
    return(wMeanLL)
  })
  dBBwCoordinates <- do.call("rbind",dBBwCoordinatesL)
  # dBBwCoordinates$date <- as_datetime(as.character(gsub("X","",dBBwCoordinates$date)), tz="UTC", format="%Y.%m.%d")
  
  
  ###############################################
  ## table of all results of UD calculations ##
  ###############################################
  # indiv <- moveObj@idData$individual.local.identifier
  # if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  dailyUDcalc <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, includedDatesNbLoc$date, sep="_"),
                            individual=indiv,
                            date=includedDatesNbLoc$date,
                            locsPerDay=includedDatesNbLoc$Freq,
                            UDsizeKm2=unlist(UDsizeL),
                            UDcentroidsLongitude=centroids[,1],
                            UDcentroidsLatitude=centroids[,2],
                            UDwMeanLongitude=dBBwCoordinates$wMeanLong,
                            UDwMeanLatitude=dBBwCoordinates$wMeanLat,
                            row.names = NULL)
  
  saveRDS(dailyUDcalc, file=paste0(pathToOutputFolder,"dailyUDcalc_",moveObj@idData$study.id,"_",indiv,".rds"))
  
  ###################################
  ## daily dBB coordinates as spdf ##
  ###################################
  dBBcoordinatesL <- lapply(1:length(dailydBBL), function(x){ 
    ud <- dailyUDL[[x]]
    dbb <- dailydBBL[[x]]
    dbb[ud>UDpercentage] <- NA
    dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
    dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
    dbb_spdf_ll$date <- as_datetime(as.character(gsub("X","",names(dailydBBL[x]))), tz="UTC", format="%Y.%m.%d")
    return(dbb_spdf_ll)
  })
  
  dBBcoordinates_spdf <- do.call("rbind",dBBcoordinatesL)
  
  saveRDS(dBBcoordinates_spdf, file=paste0(pathToOutputFolder,"dailyDBBcoordinatesSPDF_",moveObj@idData$study.id,"_",indiv,".rds"))
  # end_time <- Sys.time()
  # end_time - start_time
}



## this is how one do the raster to spdf in sf, but it probably does not make sense in this case as 
# library(stars)
# dbbSel_t_stars <- st_as_stars(dbbSel_t)
# library(sf)
# st_as_sf(dbbSel_t_stars, as_points = TRUE, merge = FALSE)






