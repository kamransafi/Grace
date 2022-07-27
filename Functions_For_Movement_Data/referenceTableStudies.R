library('move')
library('lubridate')

## get a reference table with 1 line per individual
## this function gives 1 line per individual, therefore after running this function, make a do.call("rbind") to get one large table which can be used for different filtering, e.g. duplicate individuals (in different studies)
## to this large table columns can be added with the explanation why studies got removed from the analysis

referenceTableStudies <-  function(moveObj){ 
  
  rdf <- data.frame(
    MBid = moveObj@idData$study.id,
    individual.local.identifier = namesIndiv(moveObj),
    tag.local.identifier = moveObj@idData$tag.local.identifier,
    # deployment.id = moveObj@idData$deployment.id, ## check what happens when indiv has 2 several deployments ## probably dont need this info
    species= moveObj@idData$individual.taxon.canonical.name,
    tracking_duration_days = as.numeric(round(difftime(timestamps(moveObj)[n.locs(moveObj)], timestamps(moveObj)[1], "days"))),
    tracking_start_date = timestamps(moveObj)[1],
    tracking_end_date = timestamps(moveObj)[n.locs(moveObj)],
    GPSpts_total = n.locs(moveObj),
    median_timelag_mins = round(median(timeLag(moveObj, "mins")))
  )
  return(rdf)
}

# refL <- lapply(allMv, referenceTableStudies)
# referenceTableStudies_ALL <- do.call("rbind",refL)