
load("~/Grace/NoPush/referenceTableStudies_ALL.RData")#,referenceTableStudies_ALL)

pathToFolder <- "~/.../.../" # path to folder of move object per individual

## selecting the studies esxcluded=="no"
includeStudiesTB <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",]

## making list of table per study
splitBYstudy_l <- split(includeStudiesTB, includeStudiesTB$MBid)

## creating moveObj per individual
lapply(splitBYstudy_l, function(tab){
  dfstudy <- readRDS(paste0("~/.../studyID_", tab$MBid,".rsd"))
  lapply(tab$individual.local.identifier, function(indiv){
    dfindiv <- dfstudy[dfstudy$individual.local.identifier==indiv]
    dfindiv <- dfindiv[!duplicated(paste0(dfindiv$timestamp,dfindiv$deployment.id)),] ## removing duplicates
    mv <- move(dfindiv)
    
    saveRDS(mv,file=paste0(pathToFolder,tab$MBid,"_",indiv,".rds"))
  })
})