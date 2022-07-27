library('move')
library('lubridate')

## function calculates sum of all step lenghts (in meters) per day. Output table contains commonID and cumulative distance value 
## saves table per individual called "cumDistDay_MBid_indiv.name.RData", object contained is called "cumDistDay".
cumulativeDist <-  function(moveObj, pathToFolder){ 
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){sum(distance(x))})
  distSplitTab <- do.call("rbind",distSplitL)
  cumDistDay <- data.frame(commonID=paste(moveObj@idData$study.id, namesIndiv(moveObj), row.names(distSplitTab), sep="_"),
                            cumulativeDist=distSplitTab[,1],
                            row.names = NULL)
  # return(cumDistDay)
  save(file=paste0(pathToFolder,"cumDistDay_",moveObj@idData$study.id,"_",namesIndiv(moveObj),".RData"),cumDistDay)
}