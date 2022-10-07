
#_________________________________
## Download studies locally: ####

library(tools)
library(data.table)
library(move)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")
dir.create("MovementData/RawData")

credsT <- movebankLogin() #movebank credentials
studsT <- getMovebank("study", credsT)
studsT <- studsT[which(studsT$i_am_collaborator=="true" | studsT$i_am_owner=="true" | studsT$i_have_download_access=="true"),]
studsT <- studsT[which(studsT$is_test == "false"),]
studsT <- studsT[grep("GPS", studsT$sensor_type_ids),]

write.csv(studsT, file="MovementData/availableWikelskiStudies_accessed29July2022.csv", row.names = F)

# Define function to find errors in the list after the "try"
is.error <- function(x) inherits(x, "try-error")
# Create temporary folder to store the csv data downloaded using the system call
tmpfld <- tempdir()

toDo <- which(!studsT$id %in% sapply(strsplit(list.files("MovementData/RawData"), "_"), "[", 2))
toDo <- studsT[toDo,]

# Download data per individual so that we can already filter out individuals that don't have acc information
# IMPORTANT: working in parallel doens't work for the download, use normal lapply
#results <- lapply(1:nrow(studsT), function(i) try({
  #stRow <- studsT[i,]
results <- lapply(1:nrow(toDo), function(i) try({
  stRow <- toDo[i,]
  print(paste0(stRow$id," - ",stRow$name))
  studyId <- as.numeric(stRow$id)
  # getting license terms of study
  system(paste0('curl -v -u ', paste0(as.vector(credsT$headers),collapse=":"), ' -c ./cookies.txt -o ',tmpfld,'/Movebank_license_terms.txt "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '"'))
  # download data accepting licence. After running this line once, we can use the regular move functions for download. --- CSV files name is the studyID. details about attributes, sensors, individuals, etc can/should be added
  system(paste0('curl -v -u ', paste0(as.vector(credsT$headers), collapse=":"), ' -b ./cookies.txt -o ',tmpfld,'/',paste0(studyId,".csv"),' "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '&license-md5=', md5sum(paste0(tmpfld,'/Movebank_license_terms.txt')), '"'))
  # Now the licence has been "accepted" and we can normally downloaad the rest of the information using the move functions
  allInds <- getMovebank("individual", login=credsT, study_id=studyId) #, timestamp_end=20220729235959000
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
      # Download gps data per individual
      gps <- getMovebankLocationData(study=studyId, animalName=ind, sensorID="GPS", 
                                     login=credsT, underscoreToDots=T)  #, timestamp_end=20220729235959000
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
      gpsDf <- as.data.frame(rbindlist(gps_ls, fill=T))
      saveRDS(gpsDf, file=paste0("MovementData/RawData/studyId_",studyId,"_noDups_onlyGps.rds"))
    }
  }
})) 

## Check reasons for failed download/errors:
table(vapply(results, is.error, logical(1)))
# which returned errors/messages and why? by assigning names (seq_along) we can remove list elements (the ones which did not return errors) but we maintain the original list indexing
results
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
# Check studies that returned errors:
# (gaveError <- toDo$name[vapply(results, is.error, logical(1))])
# toDoStill <- toDo[toDo$name %in% gaveError,]

didNotDownload <- toDo[c(7,18,23,24,26,38,39,40,44,45,58,63,65,69), c("id","name")]
write.csv(didNotDownload, file="MovementData/studiesThatDidNotDownaloadViaAPI.csv", row.names=F)


#____________________________________
## Manually downloaded studies: ####
# Some studies had to be downloaded manually as csv files. 
# We now import them, format them the same way as the other studies, re-save them as .rds and delete the downloaded .csv file.

fls <- list.files("MovementData/RawData", pattern=".csv", full.names = T)

lapply(fls, function(f){
  print(f)
  studyId <- sapply(strsplit(f, "_|.csv"), "[", 2)
  
  gps <- read.csv(f, as.is=T)
  gps$timestamp <- as.POSIXct(gps$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  gps$study.id <- studyId
  
  gps <- gps[complete.cases(gps[,c("timestamp","location.long","location.lat")]),]
  if(nrow(gps)>0){
    # Remove duplicates (it doesn't matter which as we only need time and coordinates)
    dups <- duplicated(gps[,c("timestamp", "location.long", "location.lat")])
    gps <- gps[!dups,]
    if(nrow(gps)>0){
      saveRDS(gps, file=paste0("MovementData/RawData/studyId_",studyId,"_noDups_onlyGps_manualDownload.rds"))
      #unlink(f)
    }}
})



#_____________________________
## Study summary table: ####
# Create a summary table per study where we write which studies were downloaded, and how many of the available individuals were actually downloaded? 

studsT <- read.csv("MovementData/availableWikelskiStudies_accessed29July2022.csv", as.is=T)

didNotDownload <- read.csv("MovementData/studiesThatDidNotDownaloadViaAPI.csv", as.is=T)
didNotDownload$studyDownloaded <- "missing individual id - re-assigned after manual download"
didNotDownload$studyDownloaded[c(4,7,9,10,11,13)] <- "yes manually"

fls <- list.files("MovementData/RawData", pattern="onlyGps.rds", full.names = T)
studies_downloadTable <- as.data.frame(rbindlist(lapply(fls, function(f){
  gps <- readRDS(f)
  print(gps$study.id[1])
  df <- data.frame(id=gps$study.id[1], 
                   studyDownloaded = "yes from API",
                   downloadedIndividuals=length(unique(gps$individual.local.identifier)), stringsAsFactors = F)
  return(df)
})))

downloadedStudies <- as.data.frame(rbindlist(list(didNotDownload[,c("id","studyDownloaded")], studies_downloadTable), fill=T))

studies_summaryTable <- merge(studsT[,c("id","name","contact_person_name","number_of_individuals")], downloadedStudies, by="id", all.x=T)
studies_summaryTable$studyDownloaded[is.na(studies_summaryTable$studyDownloaded)] <- "no - no individual with GPS data available"

studies_summaryTable <- studies_summaryTable[order(studies_summaryTable$id),c("id","name","number_of_individuals","downloadedIndividuals","studyDownloaded","contact_person_name")]

write.csv(studies_summaryTable, file="MovementData/studies_summaryTable_downloadOrNot.csv", row.names = F)

