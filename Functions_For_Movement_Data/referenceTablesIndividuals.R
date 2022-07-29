library('move')
library('lubridate')

## this function gives one table per individual. This table can be used to filter out days with "to few" locations, etc
## output is "RefTableIndiv_MBid_indiv.loc.ident.RData", object saved is called "RefTableIndiv"
referenceTable_Individuals <-  function(moveObj,pathToFolder){ 
  
  rdfi <- data.frame(
    commonID = paste0(moveObj@idData$study.id,"_",namesIndiv(moveObj),"_",floor_date(timestamps(moveObj), "day")),
    MBid = moveObj@idData$study.id,
    individual.local.identifier = namesIndiv(moveObj),
    tag.local.identifier = moveObj@idData$tag.local.identifier,
    # deployment.id = moveObj@idData$deployment.id, ## check what happens when indiv has 2 several deployments
    species= moveObj@idData$individual.taxon.canonical.name,
    date = floor_date(timestamps(moveObj), "day"),
    tracking_duration_days = as.numeric(round(difftime(timestamps(moveObj)[n.locs(moveObj)], timestamps(moveObj)[1], "days"))),
    GPSpts_total = n.locs(moveObj)
  )
  rdfi_r <- rdfi[!duplicated(rdfi), ]
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  
  rdfi_r$GPSpts_day <- unlist(lapply(moveObjSplitTime, n.locs))
  
  medTL <- unlist(
    lapply(moveObjSplitTime, function(x){
      tl <- timeLag(x, "mins")
      medianTL <- median(tl)
      return(medianTL)
    }))
  rdfi_r$median_timelag_mins_day <- round(medTL)
  RefTableIndiv <- rdfi_r
  # return(rdfi_r)
  save(file=paste0(pathToFolder,"RefTableIndiv_",moveObj@idData$study.id,"_",namesIndiv(moveObj),".RData"),RefTableIndiv)
}



