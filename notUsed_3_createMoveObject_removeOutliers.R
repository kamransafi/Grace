
library(data.table)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

referenceTableStudies_ALL <- readRDS("MovementData/referenceTableStudies_ALL_excludedColumn.rds")

studyFls <- list.files("MovementData/RawData", pattern="rds", full.name=T)
pathToFLD <- "MovementData/individualDF_noOutliers/" # path to folder where to save data per individual
pathToPlot <- "MovementData/filteringOutliers_examples/"
dir.create(pathToFLD)
dir.create(pathToPlot)

## selecting the studies exccluded=="no"
includeStudiesTB <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",]

## making list of table per study
splitBYstudy_l <- split(includeStudiesTB, as.character(includeStudiesTB$MBid))

# Define function to find errors in the list after the "try"
is.error <- function(x) inherits(x, "try-error")

#tab=splitBYstudy_l[["14671003"]]

## creating moveObj per individual splitBYstudy_l [as.numeric(names(err))]
results <- sapply(splitBYstudy_l, function(tab)try({
  
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
    # order by timestamp
    dfindiv <- dfindiv[order(dfindiv[,"timestamp"]),]
    # replace / in individual name, it gives problem when saving files
    if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
    # split by tag id (in case there is a time gap between tags) and remove outliers based on latitude and longitude
    tagLs <- split(dfindiv, dfindiv$tag.local.identifier)
    
    indSub <- as.data.frame(rbindlist(lapply(tagLs, function(tag){
      qLon <- quantile(tag$location.long, seq(0,1, 0.0005))
      qLat<- quantile(tag$location.lat, seq(0,1, 0.0001))
      tagSub <- tag[tag$location.long > qLon["0.05%"] & tag$location.long < qLon["99.95%"] &
                      tag$location.lat > qLat["0.01%"] & tag$location.lat < qLat["99.99%"],]
      if(nrow(tagSub)>0){
        png(paste0(pathToPlot,unique(tab$MBid),"_",indiv,"_",unique(tag$tag.local.identifier),".png"))
        par(mfrow=c(1,2))
        plot(location.lat~location.long, data=tag, type="l", ylim=range(tagSub$location.lat), xlim=range(tagSub$location.long))
        plot(location.lat~location.long, data=tagSub, type="l")
        dev.off()
        return(tagSub)
      }
    })))
    
    # save individual df without outliers
    if(nrow(indSub)>0){
      saveRDS(indSub, file=paste0(pathToFLD,unique(tab$MBid),"_",indiv,".rds"))
    }
  })
}))


## Check reasons for errors:
table(vapply(results, is.error, logical(1)))
# which returned errors/messages and why? by assigning names (seq_along) we can remove list elements (the ones which did not return errors) but we mantain the original list indexing
names(results) <- seq_along(results)
(err <- results[vapply(results, is.error, logical(1))])
toDo <- toDo[as.numeric(names(err))]
