library('move')
library('lubridate')

## get a reference table with 1 line per individualID-tagID combination
# So individuals which have multiple tags/deployments will have one line per tagID, associated to the start and end of the tracking time for that specific tag.
# This is important for finding duplicated tagIDs within or across studies.

referenceTableStudies <-  function(path_to_studyDf){ 
  
  studyDf <- readRDS(path_to_studyDf)
  
  indivList <- split(studyDf, as.character(studyDf$individual.local.identifier))
  
  studyTable <- as.data.frame(rbindlist(lapply(indivList, function(ind){
    
    tagList <- split(ind, as.character(ind$tag.local.identifier))
    indTable <- as.data.frame(rbindlist(lapply(tagList, function(tag){
      
      rdf <- data.frame(
        MBid = ind$study.id[1],
        individual.local.identifier = unique(ind$individual.local.identifier),
        tag.local.identifier = unique(tag$tag.local.identifier),
        species = unique(ind$individual.taxon.canonical.name),
        tracking_duration_days = as.numeric(round(difftime(tag$timestamp[nrow(tag)], tag$timestamp[1], "days"))),
        tracking_start_date = tag$timestamp[1],  # Important to keep the full timestamp, as in the same day a tag can be removed from one individual and put on another one.
        tracking_end_date = tag$timestamp[nrow(tag)],
        GPSpts_total = nrow(tag),
        median_timelag_mins = as.numeric(round(median(difftime(tag$timestamp[-1], tag$timestamp[-nrow(tag)], "mins"))))
      )
      return(rdf)
      
    })))
    return(indTable)
  })))
  return(studyTable)
}




### Old function from Anne ###

## get a reference table with 1 line per individual
## this function gives 1 line per individual, therefore after running this function, make a do.call("rbind") to get one large table which can be used for different filtering, e.g. duplicate individuals (in different studies)
## to this large table columns can be added with the explanation why studies got removed from the analysis

# referenceTableStudies <-  function(moveObj){ 
#   
#   rdf <- data.frame(
#     MBid = moveObj@idData$study.id,
#     individual.local.identifier = namesIndiv(moveObj),
#     tag.local.identifier = moveObj@idData$tag.local.identifier,
#     # deployment.id = moveObj@idData$deployment.id, ## check what happens when indiv has 2 several deployments ## probably dont need this info
#     species= moveObj@idData$individual.taxon.canonical.name,
#     tracking_duration_days = as.numeric(round(difftime(timestamps(moveObj)[n.locs(moveObj)], timestamps(moveObj)[1], "days"))),
#     tracking_start_date = floor_date(timestamps(moveObj)[1], "day"), 
#     tracking_end_date = floor_date(timestamps(moveObj)[n.locs(moveObj)],"day"),
#     GPSpts_total = n.locs(moveObj),
#     median_timelag_mins = round(median(timeLag(moveObj, "mins")))
#   )
#   return(rdf)
# }

# refL <- lapply(allMv, referenceTableStudies)
# referenceTableStudies_ALL <- do.call("rbind",refL)
