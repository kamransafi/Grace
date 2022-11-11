library('move')
library('lubridate')
library("rgeos") #gCentroid
library(data.table)

########
# FOR NEXT RUN, SPLIT UP CODE IN STEPS, AND SAVE EACH STEP. hopefully that will work without constantily breaking the code
########

# bursted move object by days
# calculates dbb variance on bursted move object
# calculates daily motion variance summarized as indicated by "functionDailyMotionVariance", saved as "dailyMotionVar_.....rds"
# removes days that have less locations than "minLocationsDay", by setting @interest==F
# calculates dBB per day looping through the splitted dbbvarburst object
# calculates ud size, centroid and weighted lat/long coords, saved as "dailyUDcalc_...rds" 
# extracts coordinates and values from dbb, in SPDF, saved as "dailyDBBcoordinatesSPDF_...rds"

# pathToMV <- "/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/927364554_Silas + - DER A7N66 (e-obs 7045).rds" #
# 18957668_Fanti_111.rds
# pathToMV <- flsMV[470]

dailydBBud <-  function(pathToMV,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage,pathToReferenceTables,pathTodailyDisplacements,minKM,minSI){
  # start_time <- Sys.time()
  print(pathToMV)
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  # if(grepl(" ", indiv)==T){indiv <- gsub(" ","",indiv)}
  
  if(n.locs(moveObj)<31){stop(paste("individual contains less than 32 locations"))}
  
  ## reading in the reference table of the individual, and the dailyDisplacement for flying animals
  pathToMVsplit <- unlist(strsplit(pathToMV, split="/"))
  indvMB <- grep(".rds", pathToMVsplit, value = TRUE) 
  # if(grepl(" ", indvMB)==T){indvMB <- gsub(" ","",indvMB)}
  refTab <- readRDS(paste0(pathToReferenceTables,"/RefTableIndiv_",indvMB))
  if(unique(refTab$Locomotion)=="flying"){
    dipslTab <- readRDS(paste0(pathTodailyDisplacements,"/dailyDisplacement_",indvMB))
    daysToExclude <- dipslTab$date[dipslTab$straightnessIndex>minSI & dipslTab$maxNetDispl_km>minKM]
  }
  if(unique(refTab$Locomotion)%in%c("unknownSps","excluded")){stop(paste("locomotion unknownSps or excluded"))}
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  daysToInclude <- locPerDayDF$roundTS[locPerDayDF$Freq>=minLocationsDay]
  if(unique(refTab$Locomotion)=="flying"){daysToInclude <- daysToInclude[!daysToInclude%in%daysToExclude]}
  if(length(daysToInclude)==0){stop(paste("individual contains 0 days with min nb of locations"))}
  dailyBurst <- burst(moveObj,f=as.character(roundTS[-length(roundTS)]))
  dailyBurst_c <- spTransform(dailyBurst,center=T)
  dailydBBvar <- brownian.motion.variance.dyn(dailyBurst_c, location.error=locationError,margin=11, window.size=31)
  
  # saveRDS(dailydBBvar, file=paste0(pathToOutputFolder,"dailydBBvar_",moveObj@idData$study.id,"_",indiv,".rds"))
  ###########################
  ## daily motion variance ## 
  ###########################
  motionVar <- data.frame(date= floor_date(timestamps(moveObj), "day"), motVar = getMotionVariance(dailydBBvar))
  aggMotionVar <- aggregate(motionVar$motVar, by=list(motionVar$date), FUN=functionDailyMotionVariance, na.rm=T)
  
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
  dailydBBvar@interest[timeLag(moveObj,"hours")>5] <- FALSE ## excluding segments longer than 5 hours from the dbbmm
  dailydBBvar@interest[!floor_date(timestamps(dailydBBvar), "day") %in% daysToInclude] <- FALSE # calc UD only on days with enough points
  dailydBBvarL <- split(dailydBBvar)
  ## removing days with all @interest==FALSE
  dailydBBvarL_sel <- lapply(dailydBBvarL, function(x){if(any(x@interest)){x}else{x <- NULL}})
  dailydBBvarL_sel <- dailydBBvarL_sel[which(!sapply(dailydBBvarL_sel, is.null))] # removing "NULL" elements from list
  
  #################
  ## daily dBBMM ##
  #################
  dailydBBL0 <- lapply(dailydBBvarL_sel, function(dBBvar){try({
  # dailydBBL0 <- lapply(1:length(dailydBBvarL_sel), function(x){try({ # removes names from putput list! and code does not work downstream!!
  #   print(x)
  #   dBBvar <- dailydBBvarL_sel[[x]]
    if(max(dBBvar@means,na.rm=T)>1000000){stop("to high variance estimates")}
    db_r <- raster(ext=extent(dBBvar)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=rasterLayer, crs=projection(dBBvar),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
    brownian.bridge.dyn(dBBvar, raster=db_r,location.error=rep(locationError,length(dBBvar)),margin=11, window.size=31, time.step=45/15, verbose=F)
  })
    })
  is.error <- function(x) inherits(x, "try-error")
  dailydBBL <- dailydBBL0[!vapply(dailydBBL0, is.error, logical(1))]
 
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
    # UDsizeKm2 <- cellStats(UDsel, 'sum') # this only works if rasterLayer==1km
    UDsizem2 <- cellStats(UDsel, 'sum')*rasterLayer*rasterLayer
    UDsizeKm2 <- UDsizem2/1000000
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
    # print(x)
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
  dBBwCoordinates <- do.call("rbind", dBBwCoordinatesL)
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
    dbb_spdf_ll$commonID <- paste(moveObj@idData$study.id, indiv, as_datetime(as.character(gsub("X","",names(dailydBBL[x]))), tz="UTC", format="%Y.%m.%d"), sep="_")
    return(dbb_spdf_ll)
  })
  
  dBBcoordinates_spdf <- do.call("rbind",dBBcoordinatesL)
  names(dBBcoordinates_spdf)[1] <- "dBBvalue"
  
  saveRDS(dBBcoordinates_spdf, file=paste0(pathToOutputFolder,"dailyDBBcoordinatesSPDF_",moveObj@idData$study.id,"_",indiv,".rds"))
  # end_time <- Sys.time()
  # end_time - start_time
}



## this is how one do the raster to spdf in sf, but it probably does not make sense in this case as 
# library(stars)
# dbbSel_t_stars <- st_as_stars(dbbSel_t)
# library(sf)
# st_as_sf(dbbSel_t_stars, as_points = TRUE, merge = FALSE)






