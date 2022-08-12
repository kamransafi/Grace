
#______________________________________________________________________________
## Create and save one move object per individual and subsample to 1 hour ####

library(move)
library(amt)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

referenceTableStudies_ALL <- readRDS("MovementData/referenceTableStudies_ALL_excludedColumn.rds")

studyFls <- list.files("MovementData/RawData", pattern="rds", full.name=T)
pathToMO <- "MovementData/MoveObjects_1hourSubsample/" # path to folder of move object per individual

## selecting the studies exccluded=="no"
includeStudiesTB <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",]

## making list of table per study
splitBYstudy_l <- split(includeStudiesTB, as.character(includeStudiesTB$MBid))

## creating moveObj per individual
results <- sapply(splitBYstudy_l, function(tab)try({
  
  #tab=splitBYstudy_l[["416289710"]] #416289710 559335125 47899041
  (f <- grep(unique(tab$MBid), studyFls, value=T))
  
  dfstudy <- readRDS(f)
  if(all(is.na(dfstudy$individual.local.identifier))==T
     & all(is.na(dfstudy$tag.local.identifier))==F){ # this is to account for when individual.local.identifier is missing
    dfstudy$individual.local.identifier <- dfstudy$tag.local.identifier}
  dfstudy$individual.local.identifier <- as.character(dfstudy$individual.local.identifier)
  dfstudy$tag.local.identifier <- as.character(dfstudy$tag.local.identifier)
  
  lapply(unique(as.character(tab$individual.local.identifier)), function(indiv){
    # One individual can have only one deployment that was excluded as duplicate. We make sure that only the non-duplicated tags for taht individual are included.
    tagsPerInd <- as.character(tab$tag.local.identifier[tab$individual.local.identifier==indiv])
    dfindiv <- dfstudy[dfstudy$individual.local.identifier==indiv & dfstudy$tag.local.identifier %in% tagsPerInd,]
    # removing duplicates created by GPRS (location not exactly equal at decimal level)
    dfindiv <- dfindiv[!duplicated(dfindiv$timestamp),]
    # Create a track object (amt package)
    indivTrack <- make_track(tbl=dfindiv, .x=location.long, .y=location.lat, .t=timestamp, all_cols=T)
    # subsample to 1 location/hour and save
    if(nrow(indivTrack)>1){
      indivTrack_1h <- track_resample(indivTrack, rate = hours(1), tolerance = minutes(15), start = 1)
      # Create move object per individual and save
      mv_1h <- as_move(indivTrack_1h)
      if(nrow(mv_1h)>1){
      if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
      saveRDS(mv_1h, file=paste0(pathToMO,unique(tab$MBid),"_",indiv,".rds"))
      }
    }
  })
}))


## Check reasons for errors:
is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
# which returned errors/messages and why? by assigning names (seq_along) we can remove list elements (the ones which did not return errors) but we mantain the original list indexing
names(results) <- seq_along(results)
(err <- results[vapply(results, is.error, logical(1))])
toDo <- toDo[as.numeric(names(err))]

# These numbers differ, nut no explicit errors... about 400 individuals missing?
# totInds <- sum(sapply(splitBYstudy_l, function(study) length(unique(study$individual.local.identifier))))
# totInds == length(list.files(pathToMO))

#_____________________
## Sanity check  ####
# Check a random samples of studies and individuals within studies 
# to make sure the minimum time lag is never below 45 min (60 min +-15 tolerance)

## creating moveObj per individual
tL <- sapply(splitBYstudy_l[sample(1:length(splitBYstudy_l),40)], function(tab)try({
  
  dfstudy <- readRDS(f)
  dfstudy$individual.local.identifier <- as.character(dfstudy$individual.local.identifier)
  dfstudy$tag.local.identifier <- as.character(dfstudy$tag.local.identifier)
  
  lapply(unique(tab$individual.local.identifier)[sample(1:length(unique(tab$individual.local.identifier)),10, replace = T)], function(indiv){
    # One individual can have only one deployment that was excluded as duplicate. We make sure that only the non-duplicated tags for taht individual are included.
    tagsPerInd <- as.character(tab$tag.local.identifier[tab$individual.local.identifier==indiv])
    dfindiv <- dfstudy[dfstudy$individual.local.identifier==indiv & dfstudy$tag.local.identifier %in% tagsPerInd,]
    # removing duplicates created by GPRS (location not exactly equal at decimal level)
    dfindiv <- dfindiv[!duplicated(dfindiv$timestamp),]
    # Create a track object (amt package)
    indivTrack <- make_track(tbl=dfindiv, .x=location.long, .y=location.lat, .t=timestamp, all_cols=T)
    # subsample to 1 location/hour and save
    indivTrack_1h <- track_resample(indivTrack, rate = hours(1), tolerance = minutes(15), start = 1)
    # Create move object per individual and save
    mv_1h <- as_move(indivTrack_1h)
    return(min(timeLag(mv_1h, "mins")))
    # plot(indivTrack)
    # points(mv_1h, col="red")
  })
}))

tL
any(tL<44)





 



