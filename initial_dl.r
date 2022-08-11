library(move)
library(fields)
library(sf)
library(stars)
library(data.table)
credsT <- movebankLogin() #movebank credentials
studsT <- getMovebank("study", credsT)
studsT <- studsT[which(studsT$i_am_collaborator=="true" | studsT$i_am_owner=="true" | studsT$i_have_download_access=="true"),]
studsT <- studsT[which(studsT$is_test == "false"),]
studsT <- studsT[grep("GPS", studsT$sensor_type_ids),]

studiesMB <- list()
success <- 0
failed <- NULL

#replace by foreach for parallel processing
for(i in 1:nrow(studsT)){
  TF <- try(getMovebankData(studsT[i,"name"], login=credsT, removeDuplicatedTimestamps=TRUE), T)
  if(class(TF) != "try-error"){
    tmp <- list()
    #here we can start including the functions for the metrics
    tmp$species <- unique(TF@idData$taxon_canonical_name)
    tmp$timelag <- unlist(lapply(timeLag(TF, units="hours"), mean))
    tmp$medianDistance <- unlist(lapply(distance(TF), median))
    
    
    studiesMB <- append(studiesMB, list(tmp))
    names(studiesMB) <- c(names(studiesMB)[-length(names(studiesMB))], studsT[i,"name"])
    #....
    print(paste("Success for ", studsT[i,"name"], ", study #", i, ".", sep=""))
    success <- success+1
    
  }else{
    print(paste(studsT[i,"name"], ", study #", i," failed.", sep=""))
    failed <- c(failed, studsT[i,"name"])
  }
}
