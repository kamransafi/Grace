library('move')
library('lubridate')

## function calculates:
# - cumulativeDist_km: sum of all step lenghts (in Km) per day.
# - maxNetDispl_km: maximum distance (in Km) between any 2 locations per day.
# - straightnessIndex: maxNetDispl_km/cumulativeDist_km. between 0-1, 1 is moving in straight line
## saves table per individual called "dailyDisplacement__MBid_indiv.name.rds"

dailyDispl <-  function(pathToMV, pathToOutputFolder){ 
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  
  moveObjSplitTime <- split(moveObj, roundTS)
  cumDistDayL <- lapply(moveObjSplitTime, function(x){sum(distance(x))})
  cumDistDay <- do.call("rbind",cumDistDayL)
  maxNetDispL <- lapply(moveObjSplitTime, function(x){max(pointDistance(coordinates(x), lonlat=T, allpairs=T),na.rm=T)})
  maxNetDisp <- do.call("rbind",maxNetDispL)
  
  dailyDisplacement <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, locPerDayDF$roundTS, sep="_"),
                           individual=indiv,
                           date=locPerDayDF$roundTS,
                           locsPerDay=locPerDayDF$Freq,
                           cumulativeDist_km=cumDistDay[,1]/1000,
                           maxNetDispl_km=maxNetDisp[,1]/1000,
                           row.names = NULL)
  dailyDisplacement$straightnessIndex <- dailyDisplacement$maxNetDispl_km/dailyDisplacement$cumulativeDist_km
  
  saveRDS(dailyDisplacement, file=paste0(pathToOutputFolder,"dailyDisplacement_",moveObj@idData$study.id,"_",indiv,".rds"))
}