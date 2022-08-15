library('move')
library('lubridate')

## function calculates sum of all step lenghts (in meters) per day. Output table contains commonID and cumulative distance value 
## saves table per individual called "cumDistDay_MBid_indiv.name.RData", object contained is called "cumDistDay".
cumulativeDist <-  function(pathToMV, pathToOutputFolder){ 
  moveObj <- readRDS(pathToMV)
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){sum(distance(x))})
  distSplitTab <- do.call("rbind",distSplitL)
  cumDistDay <- data.frame(commonID=paste(moveObj@idData$study.id, moveObj@idData$individual.local.identifier, row.names(distSplitTab), sep="_"),
                            cumulativeDist_km=distSplitTab[,1]/1000,
                            row.names = NULL)
  # return(cumDistDay)
  saveRDS(cumDistDay, file=paste0(pathToOutputFolder,"cumDistDay_",moveObj@idData$study.id,"_",moveObj@idData$individual.local.identifier,".rds"))
}