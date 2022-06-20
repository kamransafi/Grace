library('move')
library('lubridate')

## function calculates sum of all step lenghts (in meters) per day. Output table contains commonID and cumulative distance value 
cumulativeDist <-  function(moveObj){ 
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){sum(distance(x))})
  distSplitTab <- do.call("rbind",distSplitL)
  distSplitDF <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$deployment.id, row.names(distSplitTab), sep="_"),
                            cumulativeDist=distSplitTab[,1],
                            row.names = NULL)
  return(distSplitDF)
}