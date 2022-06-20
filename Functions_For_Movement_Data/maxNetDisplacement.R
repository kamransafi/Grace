library('move')
library('lubridate')
library('raster')

## function calculates the maximum distance (in meters) between any 2 locations per day. Output table contains commonID and maximum net displacement value 
maxNetDisp <-  function(moveObj){ 
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){max(pointDistance(coordinates(x), lonlat=T, allpairs=T),na.rm=T)})
  distSplitTab <- do.call("rbind",distSplitL)
  distSplitDF <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$deployment.id, row.names(distSplitTab), sep="_"),
                            maxNetDisp=distSplitTab[,1],
                            row.names = NULL)
  return(distSplitDF)
}