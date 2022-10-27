library('move')
library('lubridate')

## this function gives one table per individual, with:
# - MBid, individual,  species, date #tag,
# - tracking_duration_in_days, GPSpts_total
# - locsPerDay, median_timelag_mins_day
# - locomotion mode, and adds genus name of some that did not include it in MB
## output is "RefTableIndiv_MBid_indiv.name.rds"

# pathToMV <-  "/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers//10135184_Megan.rds"
# flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", pattern="10135184", full.names = T)
# moveObj <- readRDS(flsMV[1])     

referenceTable_Individuals <-  function(pathToMV, pathToOutputFolder,walkingGenus,swimmingGenus,flyingGenus,snakeGenus,missSps){ 
  moveObj <- readRDS(pathToMV)
  indiv <- moveObj@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  # if(grepl(" ", indiv)==T){indiv <- gsub(" ","",indiv)}
  
  RefTableIndiv0 <- data.frame(
    commonID = paste0(moveObj@idData$study.id,"_",indiv,"_",floor_date(timestamps(moveObj), "day")),
    MBid = as.character(moveObj@idData$study.id),
    individual = indiv,
    # tag.local.identifier = if(is.null(moveObj@idData$tag.local.identifier)){moveObj$tag.local.identifier},
    # deployment.id = moveObj@idData$deployment.id, ## check what happens when indiv has 2 several deployments
    species= moveObj@idData$individual.taxon.canonical.name,
    genus=  sub(" .*", "", moveObj@idData$individual.taxon.canonical.name),
    date = floor_date(timestamps(moveObj), "day"),
    tracking_duration_days = as.numeric(round(difftime(timestamps(moveObj)[n.locs(moveObj)], timestamps(moveObj)[1], "days"))),
    GPSpts_total = n.locs(moveObj)
  )
  RefTableIndiv <- RefTableIndiv0[!duplicated(RefTableIndiv0), ]
  
  ## to add missing sps and genus. "missSps" table comes from here: "/home/anne/Documents/GRACEdata/studiesWithMissingSpsName_completed.csv"
  if(unique(RefTableIndiv$MBid) %in% missSps$MBid){
    RefTableIndiv$species <- missSps$species[missSps$MBid==unique(RefTableIndiv$MBid)]
    RefTableIndiv$genus <- missSps$genus[missSps$MBid==unique(RefTableIndiv$MBid)]
  }
  ##
  
  roundTS <- floor_date(timestamps(moveObj), "day") 
  moveObjSplitTime <- split(moveObj, roundTS)
  
  RefTableIndiv$locsPerDay <- unlist(lapply(moveObjSplitTime, n.locs))
  
  medTL <- unlist(
    lapply(moveObjSplitTime, function(x){
      tl <- timeLag(x, "mins")
      medianTL <- median(tl)
      return(medianTL)
    }))
  RefTableIndiv$median_timelag_mins_day <- round(medTL)
  
  if(is.na(unique(RefTableIndiv$genus))){RefTableIndiv$Locomotion <- "unknownSps"} else {
    if(unique(RefTableIndiv$genus)%in%c("Animalia", "Aves")){RefTableIndiv$Locomotion <- "unknownSps"}
    if(unique(RefTableIndiv$species)%in%c("Rhincodon typus", "Physeter macrocephalus", "Eretmochelys imbricata", "Balaenoptera musculus", "Balaenoptera physalus")){RefTableIndiv$Locomotion <- "excluded"}
    if(unique(RefTableIndiv$genus)=="Homo"){RefTableIndiv$Locomotion <- "excluded"}
    if(unique(RefTableIndiv$genus) %in% walkingGenus){RefTableIndiv$Locomotion <- "walking"}
    if(unique(RefTableIndiv$genus) %in% swimmingGenus){RefTableIndiv$Locomotion <- "swimming"}
    if(unique(RefTableIndiv$genus) %in% flyingGenus){RefTableIndiv$Locomotion <- "flying"}
    if(unique(RefTableIndiv$genus) %in% snakeGenus){RefTableIndiv$Locomotion <- "snake"}
    if(is.null(RefTableIndiv$Locomotion)){stop(paste0("'",unique(RefTableIndiv$genus),"':"," not included in locomotion lists"))}
    # if(is.null(RefTableIndiv$Locomotion)){
    #   missgenus <- unique(RefTableIndiv$genus)
    #   return(missgenus)} # this was to find the missing genus
  }
  
  saveRDS(RefTableIndiv, file=paste0(pathToOutputFolder,"RefTableIndiv_",moveObj@idData$study.id,"_",indiv,".rds"))
}


