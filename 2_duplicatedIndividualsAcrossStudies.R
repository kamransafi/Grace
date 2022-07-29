#____________________________________
## Individuals reference table: ####
# Create a reference table, one entry per individual: ####

library(data.table)
library(plyr)
library(doParallel)
detectCores()
doParallel::registerDoParallel(5)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

source("Grace_R_GitHub/Functions_For_Movement_Data/referenceTableStudies.R")

fls <- list.files("MovementData/RawData", pattern="rds", full.names = T)

referenceTableStudies_ALL <- as.data.frame(rbindlist(llply(fls, referenceTableStudies, .parallel=T)))

saveRDS(referenceTableStudies_ALL, file="MovementData/referenceTableStudies_ALL_original.rds") ## just to making sure to have a copy that is untouch, as after this it will be modified and overwritten....  

#___________________________________________________
## Find duplicated individuals/tags across studies: ####
# this script might have to be adjusted depending on the particular issues that come up when all studies are gathered

library(DescTools) # for function %overlaps%


## adding some columns for managing the table
referenceTableStudies_ALL$rowID <- paste0("rID_",1:nrow(referenceTableStudies_ALL))
referenceTableStudies_ALL$excluded <- "no"

## adding columns by which to filter
referenceTableStudies_ALL$Ind_Tag_Sps <- paste0(referenceTableStudies_ALL$individual.local.identifier,"_",referenceTableStudies_ALL$tag.local.identifier,"_",referenceTableStudies_ALL$species)
referenceTableStudies_ALL$Tag_Sps <- paste0(referenceTableStudies_ALL$tag.local.identifier,"_",referenceTableStudies_ALL$species)

## check duplicated indv-tag-sps
# get table containing all rows of duplication
dupliTab_ITS <- referenceTableStudies_ALL[referenceTableStudies_ALL$Ind_Tag_Sps%in%referenceTableStudies_ALL$Ind_Tag_Sps[duplicated(referenceTableStudies_ALL$Ind_Tag_Sps)],] 

print(paste0(nrow(dupliTab_ITS), " rows duplicated by indiv, tag and sps"))
if(nrow(dupliTab_ITS)>1){
  # splitting to select which of the duplicateds to keep
  dupliTab_ITS_l <- split(dupliTab_ITS, dupliTab_ITS$Ind_Tag_Sps)
  toExclude_l <- lapply(dupliTab_ITS_l, function(x){
    # make matrix to find all paired combinations
    combiMatrix <- expand.grid(x$rowID,x$rowID,stringsAsFactors=F)
    colnames(combiMatrix) <- c("id1", "id2")
    combiMatrix <- combiMatrix[combiMatrix$id1!=combiMatrix$id2,] # removing comparisons to them selves (eg. ID2, ID2)
    combiMatrix <- combiMatrix[duplicated(t(apply(combiMatrix, 1, sort))),] # removing same comparison, but different order (eg. ID3-ID4 and ID4-ID3) as %overlaps% does not care about order
    combiMatrix$overlap <- FALSE
    # checking if overlap is true for all pairs
    for(i in 1:(nrow(combiMatrix))){
      if(c(x$tracking_start_date[x$rowID==combiMatrix$id1[i]], x$tracking_end_date[x$rowID==combiMatrix$id1[i]]) %overlaps% c(x$tracking_start_date[x$rowID==combiMatrix$id2[i]], x$tracking_end_date[x$rowID==combiMatrix$id2[i]]) == TRUE){
        combiMatrix[i,"overlap"] <- TRUE
      }
    }
    # not overlapping duplicated tag_sps in time: probably same(or different) study deployed tag on different individual
    overlaping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    # accounting for table having multiple rows, e.g. 2 individuals got tagged with same tag, both are duplicated
    exclude_l <- lapply(1:(nrow(overlaping)),function(i){
      x$rowID[which.min(x[x$rowID%in%overlaping[i,c("id1","id2")],"GPSpts_total"])] # if same number of gps pts, 1st one gets declared as min
    })
    exclude <- unlist(exclude_l)
    return(exclude)
  })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%toExclude] <- "yes_duplicated_Ind_Tag_Sps"
}


## check duplicated by tag-sps
# get table containing all rows of duplication
dupliTab_TS <- referenceTableStudies_ALL[referenceTableStudies_ALL$Tag_Sps%in%referenceTableStudies_ALL$Tag_Sps[duplicated(referenceTableStudies_ALL$Tag_Sps)],] 
dupliTab_TS <- dupliTab_TS[dupliTab_TS$excluded=="no",] ## removing those that have been already identified by duplicated_Ind_Tag_Sps

print(paste0(nrow(dupliTab_TS), " rows duplicated by tag and sps"))
if(nrow(dupliTab_TS)>1){
  # splitting to select which of the duplicateds to keep
  dupliTab_TS_l <- split(dupliTab_TS, dupliTab_TS$Tag_Sps)
  toExclude_l <- lapply(dupliTab_TS_l, function(x){
    # make matrix to find all paired combinations
    combiMatrix <- expand.grid(x$rowID,x$rowID,stringsAsFactors=F)
    colnames(combiMatrix) <- c("id1", "id2")
    combiMatrix <- combiMatrix[combiMatrix$id1!=combiMatrix$id2,] # removing comparisons to them selves (eg. ID2, ID2)
    combiMatrix <- combiMatrix[duplicated(t(apply(combiMatrix, 1, sort))),] # removing same comparison, but different order (eg. ID3-ID4 and ID4-ID3) as %overlaps% does not care about order
    combiMatrix$overlap <- FALSE
    # checking if overlap is true for all pairs
    for(i in 1:(nrow(combiMatrix))){
      if(c(x$tracking_start_date[x$rowID==combiMatrix$id1[i]], x$tracking_end_date[x$rowID==combiMatrix$id1[i]]) %overlaps% c(x$tracking_start_date[x$rowID==combiMatrix$id2[i]], x$tracking_end_date[x$rowID==combiMatrix$id2[i]]) == TRUE){
        combiMatrix[i,"overlap"] <- TRUE
      }
    }
    # not overlapping duplicated tag_sps in time: probably same(or different) study deployed tag on different individual
    overlaping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    # accounting for table having multiple rows, e.g. 2 individuals got tagged with same tag, both are duplicated
    exclude_l <- lapply(1:(nrow(overlaping)),function(i){ 
      x$rowID[which.min(x[x$rowID%in%overlaping[i,c("id1","id2")],"GPSpts_total"])] # if same number of gps pts, 1st one gets declared as min
    })
    exclude <- unlist(exclude_l)
    return(exclude)
    
  })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%toExclude] <- "yes_duplicated_Tag_Sps"
}

save(file="~/Grace/NoPush/referenceTableStudies_ALL.RData",referenceTableStudies_ALL)



