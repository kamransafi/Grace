library('move')
library('lubridate')
library('raster')

## function calculates the maximum distance (in meters) between any 2 locations per day. Output table contains commonID and maximum net displacement value 
## saves table per individual called "maxNetDisplDay_MBid_indiv.name.RData", object contained is called "maxNetDisplDay_".
maxNetDisp <-  function(moveObj,pathToFolder){ 
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){max(pointDistance(coordinates(x), lonlat=T, allpairs=T),na.rm=T)})
  distSplitTab <- do.call("rbind",distSplitL)
  maxNetDisplDay <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$deployment.id, row.names(distSplitTab), sep="_"),
                            maxNetDispl=distSplitTab[,1],
                            row.names = NULL)
  # return(maxNetDisplDay_)
  save(file=paste0(pathToFolder,"maxNetDisplDay_",moveObj@idData$study.id,"_",namesIndiv(moveObj),".RData"),maxNetDisplDay_)
}