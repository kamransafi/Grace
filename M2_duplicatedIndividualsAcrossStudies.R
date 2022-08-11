#____________________________________
## Individuals reference table: ####
# Create a reference table, one entry per individual: ####

library(data.table)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

source("Grace_R_GitHub/Functions_For_Movement_Data/referenceTableStudies.R")

fls <- list.files("MovementData/RawData", pattern="rds", full.names = T)

referenceTableStudies_ALL <- as.data.frame(rbindlist(lapply(fls, referenceTableStudies)))

saveRDS(referenceTableStudies_ALL, file="MovementData/referenceTableStudies_ALL_original.rds") ## just to making sure to have a copy that is untouch, as after this it will be modified and overwritten....  

# length(unique(referenceTableStudies_ALL$MBid))==length(fls)
# ids <- sapply(strsplit(fls,"_"), "[[", 2)
# fls[!ids %in% unique(referenceTableStudies_ALL$MBid)]

#_______________________________________________________________
## Find duplicated individuals/tags within/across studies: ####
# this script might have to be adjusted depending on the particular issues that come up when all studies are gathered

library(DescTools) # for function %overlaps%

## adding some columns for managing the table
referenceTableStudies_ALL$rowID <- paste0("rID_",1:nrow(referenceTableStudies_ALL))
referenceTableStudies_ALL$excluded <- "no"

## adding columns by which to filter
referenceTableStudies_ALL$Ind_Tag_Sps <- paste0(referenceTableStudies_ALL$individual.local.identifier,"_",referenceTableStudies_ALL$tag.local.identifier,"_",referenceTableStudies_ALL$species)
referenceTableStudies_ALL$Tag_Sps <- paste0(referenceTableStudies_ALL$tag.local.identifier,"_",referenceTableStudies_ALL$species)
referenceTableStudies_ALL$Stu_Ind_Sps <- paste0(referenceTableStudies_ALL$MBid,"_",referenceTableStudies_ALL$individual.local.identifier,"_",referenceTableStudies_ALL$species)

## check duplicated indv-tag-sps ----
# get table containing all rows of duplication
dupliTab_ITS <- referenceTableStudies_ALL[referenceTableStudies_ALL$Ind_Tag_Sps %in% referenceTableStudies_ALL$Ind_Tag_Sps[duplicated(referenceTableStudies_ALL$Ind_Tag_Sps)],] 

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
    overlapping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    # accounting for table having multiple rows, e.g. 2 individuals got tagged with same tag, both are duplicated
    exclude_l <- lapply(1:(nrow(overlapping)),function(i){
      rowToExclude <- which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])  # if same number of gps pts, 1st one gets declared as min
      x$rowID[x$rowID %in% overlapping[i,c("id1","id2")]][rowToExclude] # this works also when overlapping has multiple rows
    })
    exclude <- unlist(exclude_l)
    return(exclude)
  })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID %in% toExclude] <- "yes_duplicated_Ind_Tag_Sps"
}


## check duplicated by tag-sps ----
# get table containing all rows of duplication
dupliTab_TS <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",] ## removing those that have been already identified by duplicated Ind_Tag_Sps
dupliTab_TS <- dupliTab_TS[dupliTab_TS$Tag_Sps %in% dupliTab_TS$Tag_Sps[duplicated(dupliTab_TS$Tag_Sps)],] 
# dupliTab_TS <- referenceTableStudies_ALL[referenceTableStudies_ALL$Tag_Sps %in% referenceTableStudies_ALL$Tag_Sps[duplicated(referenceTableStudies_ALL$Tag_Sps)],] 
# dupliTab_TS <- dupliTab_TS[dupliTab_TS$excluded=="no",] ## removing those that have been already identified by duplicated Ind_Tag_Sps


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
    overlapping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    # accounting for table having multiple rows, e.g. 2 individuals got tagged with same tag, both are duplicated
    if(nrow(overlapping) > 0){
      exclude_l <- lapply(1:(nrow(overlapping)),function(i){
        rowToExclude <- which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])  # if same number of gps pts, 1st one gets declared as min
        x$rowID[x$rowID %in% overlapping[i,c("id1","id2")]][rowToExclude] # this works also when overlapping has multiple rows
      })
      exclude <- unlist(exclude_l)
      return(exclude)
    }
    })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID %in% toExclude] <- "yes_duplicated_Tag_Sps"
}

saveRDS(referenceTableStudies_ALL, file="MovementData/referenceTableStudies_ALL_excludedColumn.rds")


## check individuals with multiple tags simultaneously ----
# get table containing all rows of duplication
dupliTab_I <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",] ## removing those that have been already identified by duplicated Tag_Sps
dupliTab_I <- dupliTab_I[dupliTab_I$Stu_Ind_Sps %in% dupliTab_I$Stu_Ind_Sps[duplicated(dupliTab_I$Stu_Ind_Sps)],] 

print(paste0(nrow(dupliTab_I), " rows duplicated by individual"))
if(nrow(dupliTab_I)>1){
  # splitting to select which of the duplicateds to keep
  dupliTab_I_l <- split(dupliTab_I, dupliTab_I$Stu_Ind_Sps)
  toExclude_l <- lapply(dupliTab_I_l, function(x){
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
    overlapping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    if(nrow(overlapping) > 0){
      exclude_l <- lapply(1:(nrow(overlapping)),function(i){
        rowToExclude <- which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])  # if same number of gps pts, 1st one gets declared as min
        x$rowID[x$rowID %in% overlapping[i,c("id1","id2")]][rowToExclude] # this works also when overlapping has multiple rows
      })
      exclude <- unlist(exclude_l)
      return(exclude)
    }
  })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID %in% toExclude] <- "yes_duplicated_Study-Individual-Species"
}

saveRDS(referenceTableStudies_ALL, file="MovementData/referenceTableStudies_ALL_excludedColumn.rds")




#_____________________
# ## Alternative from excluding individual with less locations, we can also exclude individual with shorter tracking duration
# exclude_l <- lapply(1:(nrow(overlapping)),function(i){ 
#   # if both tags have same tracking duration we take the one with more GPS locations, otherwise the one with longer duration
#   if(x[x$rowID %in% overlapping[i,"id1"], "tracking_duration_days"] == x[x$rowID %in% overlapping[i,"id2"], "tracking_duration_days"]){
#     x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])]
#   }else{x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "tracking_duration_days"])] # if same number of gps pts, 1st one gets declared as min
#   }
# })



#_____________________
## Sanity checks 1 ####
# Plot the duplicated tags to make sure it's the same individual

fls <- list.files("MovementData/RawData", full.names = T)

table(referenceTableStudies_ALL$excluded)
(dups <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded %in% c("yes_duplicated_Ind_Tag_Sps","yes_duplicated_Tag_Sps"), c("MBid","Tag_Sps")])
row.names(dups) <- 1:nrow(dups)
check <- referenceTableStudies_ALL[referenceTableStudies_ALL$Tag_Sps %in% dups$Tag_Sps,]
check[,c("Ind_Tag_Sps","tracking_start_date","tracking_end_date")]

dups
#Is <- c(101,107,120,150,160,170,190,210,242,257,260,280,310,330,344,390,462,480,495,508,520,540,570,600,612,620,650,670,700,900,1209,1218,1230,1260)
i=619
#i=620 does not seam a real duplicate!!
#i=1260 and 1263 are not real duplicates but i=1264 are
check[check$Tag_Sps==dups$Tag_Sps[i],c("MBid","Ind_Tag_Sps","tracking_start_date","tracking_end_date","GPSpts_total","excluded")]

studyIDs <- paste(unique(check$MBid[check$Tag_Sps==dups$Tag_Sps[i]]), collapse="|")
study_l <- lapply(grep(studyIDs, fls, value=T), readRDS)

gpsSub <- rbindlist(lapply(study_l, function(x){
  x$Ind_Tag_Sps <- paste0(x$individual.local.identifier,"_",x$tag.local.identifier,"_",x$individual.taxon.canonical.name)
  x$Tag_Sps <- paste0(x$tag.local.identifier,"_",x$individual.taxon.canonical.name)
  x$Stud_Ind_Tag_Sps <- paste0(x$study.id,"_",x$individual.local.identifier,"_",x$tag.local.identifier,"_",x$individual.taxon.canonical.name)
  xSub <- x[x$Tag_Sps == dups$Tag_Sps[i], c("Stud_Ind_Tag_Sps","Ind_Tag_Sps","Tag_Sps","timestamp","location.long","location.lat","tag.local.identifier")]
  return(xSub)
}))
gps_l <- split(gpsSub, as.character(gpsSub$Stud_Ind_Tag_Sps))

plot(location.lat~location.long, data=gpsSub, type="n")
cols <- c("black","red","blue","green","orange","yellow","magenta","brown")
lapply(1:length(gps_l), function(p){
  lines(location.lat~location.long, data=gps_l[[p]][order(gps_l[[p]]$timestamp),], col=cols[p])
})



#_____________________
## Sanity checks 2 ####
# Plot different tags on the same individual to make sure they overlap and are actual duplicates at least in part

fls <- list.files("MovementData/RawData", full.names = T)

table(referenceTableStudies_ALL$excluded)
(dups <- as.character(referenceTableStudies_ALL$Stu_Ind_Sps[referenceTableStudies_ALL$excluded == "yes_duplicated_Study-Individual-Species"]))
check <- referenceTableStudies_ALL[referenceTableStudies_ALL$Stu_Ind_Sps %in% dups,]
check[,c("individual.local.identifier","tracking_start_date","tracking_end_date")]

studyIDs <- sapply(strsplit(dups, "_"), "[",1)

#i=136,137 "7023813_2049_Columba livia","7023813_2081_Columba livia" (tracks overlap but look different)
dups
i=152
check[check$Stu_Ind_Sps==dups[i],c("individual.local.identifier","tracking_start_date","tracking_end_date","GPSpts_total","excluded")]

gps <- readRDS(grep(studyIDs[i], fls, value=T))

gps$Stu_Ind_Sps <- paste0(gps$study.id,"_",gps$individual.local.identifier,"_",gps$individual.taxon.canonical.name)

gpsSub <- gps[gps$Stu_Ind_Sps == dups[i],]
gps_l <- split(gpsSub, as.character(gpsSub$tag.local.identifier))

plot(location.lat~location.long, data=gpsSub, type="n")
cols <- c("black","red","blue","green","orange","yellow","magenta","brown")
lapply(1:length(gps_l), function(p){
  lines(location.lat~location.long, data=gps_l[[p]][order(gps_l[[p]]$timestamp),], col=cols[p])
  print(range(gps_l[[p]][order(gps_l[[p]]$timestamp),"timestamp"]))
})


#_____________________________
## Mark excluded studies ####
# In the study summary table, where we notes which studies were downloaded, we mark which were excluded because of duplicates:

studies_summaryTable <- read.csv("MovementData/studies_summaryTable_downloadOrNot.csv", as.is=T)

keptStudies <- unique(referenceTableStudies_ALL$MBid[referenceTableStudies_ALL$excluded=="no"])
duplicatedStudies <- unique(referenceTableStudies_ALL$MBid)[!unique(referenceTableStudies_ALL$MBid) %in% keptStudies]

studies_summaryTable$studyDuplicated <- "no"
studies_summaryTable$studyDuplicated[studies_summaryTable$id %in% as.character(duplicatedStudies)] <- "yes all individual in study are duplicates"
studies_summaryTable$studyDuplicated[studies_summaryTable$studyDownload %in% unique(grep("no ",studies_summaryTable$studyDownload,value=T))] <- NA

write.csv(studies_summaryTable, "MovementData/studies_summaryTable_downloadOrNot_duplicateOrNot.csv", row.names=F)

