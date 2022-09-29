library('move')
library('lubridate')

## function calculates sum of all step lenghts (in meters) per day. Output table contains commonID and cumulative distance value 
## saves table per individual called "cumDistDay_MBid_indiv.name.RData", object contained is called "cumDistDay".
cumulativeDist <-  function(pathToMV, pathToOutputFolder){ 
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  
  moveObjSplitTime <- split(moveObj, roundTS)
  distSplitL <- lapply(moveObjSplitTime, function(x){sum(distance(x))})
  distSplitTab <- do.call("rbind",distSplitL)
  cumDistDay <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, locPerDayDF$roundTS, sep="_"),
                           individual=indiv,
                           date=locPerDayDF$roundTS,
                           locsPerDay=locPerDayDF$Freq,
                           cumulativeDist_km=distSplitTab[,1]/1000,
                           row.names = NULL)
  saveRDS(cumDistDay, file=paste0(pathToOutputFolder,"cumDistDay_",moveObj@idData$study.id,"_",indiv,".rds"))
}