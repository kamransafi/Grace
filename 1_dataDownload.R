
#_________________________________
## Download studies locally: ####

library(tools)
library(data.table)
library(move)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")
dir.create("MovementData/RawData")

credsT <- movebankLogin("TeamWikelski", "e8kF*sdB") #movebank credentials
studsT <- getMovebank("study", credsT)
studsT <- studsT[which(studsT$i_am_collaborator=="true" | studsT$i_am_owner=="true" | studsT$i_have_download_access=="true"),]
studsT <- studsT[which(studsT$is_test == "false"),]
studsT <- studsT[grep("GPS", studsT$sensor_type_ids),]

write.csv(studsT, file="MovementData/availableWikelskiStudies_accessed26July2022.csv", row.names = F)

# Define function to find errors in the list after the "try"
is.error <- function(x) inherits(x, "try-error")
# Create temporary folder to store the csv data downloaded using the system call
tmpfld <- tempdir()

# Download data per individual so that we can already filter out individuals that don't have acc information
# IMPORTANT: working in parallel doens't work for the download, use normal lapply
#results <- lapply(101:300, function(i) try({
#results <- lapply(1:nrow(studsT), function(i) try({
  #stRow <- studsT[i,]
results <- lapply(1:nrow(toDo), function(i) try({
  stRow <- toDo[i,]
  print(paste0(stRow$id," - ",stRow$name))
  studyId <- as.numeric(stRow$id)
  # getting license terms of study
  system(paste0('curl -v -u ', paste0(as.vector(credsT$headers),collapse=":"), ' -c ./cookies.txt -o ',tmpfld,'/Movebank_license_terms.txt "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '"'))
  # download data accepting licence. After running this line once, we can use the regular move functions for download. --- CSV files name is the studyID. details about attributes, sensors, individuals, etc can/should be added
  system(paste0('curl -v -u ', paste0(as.vector(credsT$headers), collapse=":"), ' -b ./cookies.txt -o ',tmpfld,'/',paste0(studyId,".csv"),' "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '&license-md5=', md5sum("./license_terms.txt"), '"'))
  # Now the licence has been "accepted" and we can normally downloaad the rest of the information using the move functions
  allInds <- getMovebank("individual", login=credsT, study_id=studyId)
  # Exclude individuals that have no GPS data
  allInds <- allInds[grep("gps", allInds$sensor_type_ids),]
  # Exclude potential testing individuals
  allInds <- allInds[which(!allInds$local_identifier %in% grep("test|Test", allInds$local_identifier, value=T)),]
  # For the remaining individuals, download the data one by one
  if(nrow(allInds) == 0){print("No GPS individuals to download for this study.")
    }else if(nrow(allInds) > 0){
    indNames <- as.character(allInds$local_identifier[!allInds$local_identifier %in% c(NA,"")])
    gps_ls <- lapply(indNames, function(ind)try({
      print(ind)
      # Download gps and check if data are from ornitela or eobs tags
      gps <- getMovebankLocationData(study=studyId, animalName=ind, sensorID="GPS", login=credsT, underscoreToDots=T)
      # Remove NAs from timestamp and coords
      gps <- gps[complete.cases(gps[,c("timestamp","location.long","location.lat")]),]
      if(nrow(gps)>0){
        # Remove duplicates (it doesn't matter which as we only need time and coordinates)
        dups <- duplicated(gps[,c("timestamp", "location.long", "location.lat")])
        gps <- gps[!dups,]
        if(nrow(gps)>0){return(gps)}}
    }))
    # Remove potential individuals that returned errors during download
    gps_ls <- gps_ls[!vapply(gps_ls, is.error, logical(1))]
    # Exclude empty elements from the list
    gps_ls <- gps_ls[which(!sapply(gps_ls, is.null))]
    # Save data
    if(length(gps_ls)>0){
      gpsDf <- as.data.frame(rbindlist(gps_ls))
      saveRDS(gpsDf, file=paste0("MovementData/RawData/studyId_",studyId,"_noDups_onlyGps.rds"))
    }
  }
})) 

## Check reasons for failed download/errors:
table(vapply(results, is.error, logical(1)))
# which returned errors/messages and why? by assigning names (seq_along) we can remove list elements (the ones which did not return errors) but we mantain the original list indexing
results
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
#manually accept licence agreements for these studies:
(licenceToAgree <- studsT$name[101:300][vapply(results, is.error, logical(1))])
toDo <- studsT[studsT$name %in% licenceToAgree,]

# notDone <- which(!studsT$id[31:100] %in% sapply(strsplit(list.files("MovementData/RawData"), "_"), "[", 2))
# toDo <- studsT[31:100,][notDone,]


#_____________________________
## Study summary table: ####
# Create a summary table per study where we write which studies were downloaded, and how many of the available individuals were actually downloaded? 

fls <- list.files("MovementData/RawData", pattern="rds", full.names = T)

studies_summTable <- as.data.frame(rbindlist(lapply(fls, function(f){
  gps <- readRDS(f)
  print(gps$study.id[1])
  allInds <- getMovebank("individual", login=credsT, study_id=gps$study.id[1])
  #print(paste0("Downloaded ",length(unique(gps$individual.local.identifier)),"/", nrow(allInds)," individuals."))
  df <- data.frame(id=gps$study.id[1], downloadedIndividuals=length(unique(gps$individual.local.identifier)), 
                   totalIndividuals=nrow(allInds), studyDownloaded="yes")
  return(df)
})))

studiesSub <- studsT[,c("id","name","contact_person_name","number_of_individuals")]

studies_summaryTable <- merge(studiesSub, studies_summTable, by="id", all.x=T)
write.csv(studies_summaryTable, file="MovementData/studies_download_filteringSteps.csv", row.names = F)

