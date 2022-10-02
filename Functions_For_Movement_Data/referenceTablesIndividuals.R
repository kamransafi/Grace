library('move')
library('lubridate')

## this function gives one table per individual, with:
# - MBid, individual,  species, date #tag,
# - tracking_duration_in_days, GPSpts_total
# - GPSSpts_day, median_timelag_mins_day
## output is "RefTableIndiv_MBid_indiv.name.rds"

# pathToMV <-  "/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers//10006517_996-92355.rds"    

referenceTable_Individuals <-  function(pathToMV,pathToOutputFolder){ 
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  
  RefTableIndiv0 <- data.frame(
    commonID = paste0(moveObj@idData$study.id,"_",indiv,"_",floor_date(timestamps(moveObj), "day")),
    MBid = moveObj@idData$study.id,
    individual = indiv,
    # tag.local.identifier = if(is.null(moveObj@idData$tag.local.identifier)){moveObj$tag.local.identifier},
    # deployment.id = moveObj@idData$deployment.id, ## check what happens when indiv has 2 several deployments
    species= moveObj@idData$individual.taxon.canonical.name,
    date = floor_date(timestamps(moveObj), "day"),
    tracking_duration_days = as.numeric(round(difftime(timestamps(moveObj)[n.locs(moveObj)], timestamps(moveObj)[1], "days"))),
    GPSpts_total = n.locs(moveObj)
  )
  RefTableIndiv <- RefTableIndiv0[!duplicated(RefTableIndiv0), ]
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  
  RefTableIndiv$GPSpts_day <- unlist(lapply(moveObjSplitTime, n.locs))
  
  medTL <- unlist(
    lapply(moveObjSplitTime, function(x){
      tl <- timeLag(x, "mins")
      medianTL <- median(tl)
      return(medianTL)
    }))
  RefTableIndiv$median_timelag_mins_day <- round(medTL)
  
  saveRDS(RefTableIndiv, file=paste0(pathToOutputFolder,"RefTableIndiv_",moveObj@idData$study.id,"_",indiv,".rds"))
}


