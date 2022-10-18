
### to get the weighted daily GRACE per indiv, 
# 1- annotate the "dailyDBBcoordinatesSPDF_.......rds" with the correct grace layer, 
# 2- than multiply the extracted grace value by the column "layer" (dBBvalue), 
# 3- and sum it up per day -> this value is the weighted mean grace per day

library(raster)
library(rgdal)
library(data.table)
library(lubridate)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

# Import list of GRACE stacks
atlGrace_infoBr <- readRDS("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.rds")
atlGrace_br <- brick("RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.tif")
names(atlGrace_br) <- names(atlGrace_infoBr)
atlGrace_br@z <- atlGrace_infoBr@z
# Import table with the GRACE collection periods
graceTimes <- readRDS("RemoteSensingData/grace_tws_collectionPeriods.rds")
graceTimes <- graceTimes[order(graceTimes$grace_time),]
graceTimes$grace_seq <- 1:nrow(graceTimes)
# List individual files containing infos of daily UDs
uds_indDay <- list.files("MovementData/MovementMetrics/nonFlying_dBB_spdf", full.names = T)

# Create directory to store result
dir.create("MovementData/MovementMetrics/nonFlying_dBB_graceExperienced")


#_______________
## step 1 and 2: extract values from the correct grace layer and multiply it by the dbb value per pixel

ud_grace_fls <- list.files("MovementData/MovementMetrics/nonFlying_dBB_graceExperienced", full.names = T)
flsToDo <- uds_indDay[!gsub(".*SPDF_|.rds","",uds_indDay) %in% gsub(".*grace_|.rds","",ud_grace_fls)]

results <- lapply(flsToDo, function(fl)try({
  print(fl)
  ud <- readRDS(fl)
  MBid_indiv <- gsub(".*dailyDBBcoordinatesSPDF_|.rds","",fl)
  # add empty columns for grace times
  ud@data[,c("grace_layer","start_graceCollection","end_graceCollection","grace_seq")] <- as.character(NA)
  
  # subset the ud dataset to only the time range in which GRACE was collected
  #range(ud@data$date); min(graceTimes$start_collection); max(graceTimes$end_collection)
  ud <- ud[ud@data$date >= min(graceTimes$start_collection) & 
             ud@data$date <= max(graceTimes$end_collection),]
  if(nrow(ud)>0){
    # for days within the collection period difference in time = 0 and inCollectionnPeriod="yes", 
    # for days outside the collection period we will measure time difference in days and inCollectionnPeriod="no"
    ud$timeDiff_daysFromCollection <- 0
    ud$inCollectionPeriod <- "yes"
    
    for(i in 1:nrow(ud)){
      minTimeDiff <- which.min(abs(difftime(ud$date[i], graceTimes$grace_time, units="days")))
      closerGrace <- graceTimes[minTimeDiff,]
      # if the daily ud is not comprised in the collection period of the closest grace layer
      # a time difference in days from the start/end of the closest collection period is included in the dataframe
      if(ud$date[i] < closerGrace$start_collection | 
         ud$date[i] > closerGrace$end_collection){
        ud$timeDiff_daysFromCollection[i] <- min(abs(difftime(ud$date[i], 
                                                              c(closerGrace$start_collection,closerGrace$end_collection), units="days")))
        ud$inCollectionPeriod[i] <- "no"
      }
      # otherwise the time difference stays as 0.
      # This way all observation dates are always associated to a grace layer (even if farther away in time) 
      ud[i,c("grace_layer","start_graceCollection","end_graceCollection","grace_seq")] <- as.matrix(closerGrace[,c("grace_time","start_collection","end_collection","grace_seq")])
    }  
    # there should be no NAs because all ud days should be associated to one grace layer
    print(table(is.na(ud@data$grace_layer)))
    print(table(ud$inCollectionPeriod))
    # For the next step we need the raster brick with all the GRACE layers, we will extract values from the layer corresponding to each day
    # step 1: DBB pixels in days that could not be associated to any grace layer are automatically excluded from the output dataframe
    ud_graceLs <- split(ud, ud$grace_layer)
    
    ud_grace <- as.data.frame(rbindlist(lapply(ud_graceLs, function(ud_gr){
      graceLayer <- atlGrace_br[[which(atlGrace_br@z$Date == unique(ud_gr$grace_layer))]]
      ud_gr$grace_tws <- extract(graceLayer, ud_gr, method="bilinear") 
      return(as.data.frame(ud_gr))
    })))
    # step 2: multiply the grace value by the dBBvalue (occurrence) to obtain the grace experienced in each DBB pixel
    ud_grace$graceExperienced <- ud_grace$dBBvalue * ud_grace$grace_tws
    
    # save output (still multiple rows/pixels per day)
    saveRDS(ud_grace, file=paste0("MovementData/MovementMetrics/nonFlying_dBB_graceExperienced/dailyDBBgrace_",MBid_indiv,".rds"))
  }
}))
## Check reasons for errors:
is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
# no errors, the movement metrics were simply outside the grace collection periods


##old script that would exclude days that are outside the collection period:
# # assign each daily ud (each row) to the grace layer-collection it belongs to
# for(i in 1:nrow(ud)){
#   minTimeDiff <- which.min(abs(difftime(ud$date[i], graceTimes$grace_time, units="days")))
#   closerGrace <- graceTimes[c(minTimeDiff-1,minTimeDiff,minTimeDiff+1),]
#   whichCollection <- ud$date[i] >= closerGrace$start_collection & 
#     ud$date[i] <= closerGrace$end_collection
#   # if a pixel/day is close to a layer but it's not comprised in the collection period the step is skipped (no grace layer is associated to that day in the UD dataframe)
#   # otherwise the grace collection period including the day will be associated to the UD dataframe
#   # should there be more than one layer to associate we select the first one (by adding [1,] to the closerGrace selection)
#   if(any(whichCollection[!is.na(whichCollection)]==T)){
#     ud[i,c("grace_layer","start_graceCollection","end_graceCollection")] <- as.matrix(closerGrace[whichCollection,c("grace_time","start_collection","end_collection")][1,])
#   }
# }
# check that all days assign to a grace layers are actually included in the data collection period
# print(all(ud@data$date > ud@data$start_graceCollection & 
#             ud@data$date < ud@data$end_graceCollection, na.rm=T))

#___________
## step 3: sum the grace experienced in each DBB pixel per day, to obtain one value of grace experienced per day

# List files with grace experienced per DBB pixel per day
ud_grace_fls <- list.files("MovementData/MovementMetrics/nonFlying_dBB_graceExperienced", full.names = T)

all_indivDailyGraceLS <- lapply(ud_grace_fls, function(f)try({
  udg <- readRDS(f)
  udg_day <- cbind(aggregate(cbind(dBBvalue, graceExperienced) ~ commonID + date, data=udg, FUN=sum, na.action=na.pass),
                  aggregate(cbind(grace_layer,start_graceCollection,end_graceCollection,grace_seq,timeDiff_daysFromCollection,inCollectionPeriod) ~ commonID + date, data=udg, FUN=unique, na.action=na.pass)[,-(1:2)])
  return(udg_day)
}))

is.error <- function(x) inherits(x, "try-error")
## Check reasons for errors:
table(vapply(all_indivDailyGraceLS, is.error, logical(1)))
# which returned errors/messages and why? by assigning names (seq_along) we can remove list elements (the ones which did not return errors) but we maintain the original list indexing
names(all_indivDailyGraceLS) <- seq_along(all_indivDailyGraceLS)
all_indivDailyGraceLS[vapply(all_indivDailyGraceLS, is.error, logical(1))]

# rbind the list and save
all_indivDailyGraceExperienced <- as.data.frame(rbindlist(all_indivDailyGraceLS))
saveRDS(all_indivDailyGraceExperienced, file="MovementData/MovementMetrics/nonFlying_allDailyGraceExperienced.rds")



