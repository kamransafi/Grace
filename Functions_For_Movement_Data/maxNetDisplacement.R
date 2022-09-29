library('move')
library('lubridate')
library('raster')

## function calculates the maximum distance (in meters) between any 2 locations per day. Output table contains commonID and maximum net displacement value 
## saves table per individual called "maxNetDisplDay_MBid_indiv.name.RData", object contained is called "maxNetDisplDay_".
maxNetDisp <-  function(pathToMV, pathToOutputFolder){ 
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){max(pointDistance(coordinates(x), lonlat=T, allpairs=T),na.rm=T)})
  distSplitTab <- do.call("rbind",distSplitL)
  maxNetDisplDay <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, locPerDayDF$roundTS, sep="_"),
                               individual=indiv,
                               date=locPerDayDF$roundTS,
                               locsPerDay=locPerDayDF$Freq,
                               maxNetDispl_km=distSplitTab[,1]/1000,
                               row.names = NULL)
  saveRDS(maxNetDisplDay, file=paste0(pathToOutputFolder,"maxNetDisplDay_",moveObj@idData$study.id,"_",indiv,".rds"))
}