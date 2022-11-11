#################################
## daily Displacements tables ###
#################################

dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/") #only once
dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements")

library(doParallel)
library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

source("Functions_For_Movement_Data/dailyDisplacements.R")

is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", full.names = T)
pathToOutputFolder <- "/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/"

start_time <- Sys.time()
results <- llply(flsMV, function(f)try({dailyDispl(f, pathToOutputFolder)})
                  ,.parallel = T)

saveRDS(results, file=paste0("/home/anne/Documents/GRACEdata/MovementMetrics/","dailyDisplacement_listWerros",".rds"))

end_time <- Sys.time()
end_time - start_time # 30mins

table(vapply(results, is.error, logical(1))) # Check potential errors:

##########
## some exploratory plots 
#####
library(ggplot2)

flsDipl <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/", full.names = T)

displL <- lapply(flsDipl, function(x){
  readRDS(x)
})

displDF <- dplyr::bind_rows(displL) 
head(displDF)

flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
refIndvL <- lapply(flsReftInd, function(x){
  readRDS(x)
})
refIndvDF <- dplyr::bind_rows(refIndvL) 
head(refIndvDF)


bigDF <- merge(refIndvDF,displDF, by=c("commonID", "individual", "date", "locsPerDay"))

head(bigDF)
hist(bigDF$locsPerDay)
# hist(randDF$straightnessIndex)#, breaks="FD")
# plot(randDF$cumulativeDist_km~randDF$straightnessIndex)

ggplot(bigDF[bigDF$locsPerDay>=8,])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8,])+geom_histogram(aes(straightnessIndex),bins=100)+facet_wrap(~Locomotion, scales = "free_y")

ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerDay>=8,])+geom_histogram(aes(cumulativeDist_km),bins=100)+xlim(0,500)
ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerDay>=8,])+geom_histogram(aes(cumulativeDist_km),bins=100)+xlim(0,500)+facet_wrap(~Locomotion, scales = "free")

sort(table(bigDF$genus),decreasing=T)

ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Ciconia",])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Chelonoidis",])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Cervus",])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Geronticus",])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Larus",])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerDay>=8 & bigDF$genus=="Aquila",])+geom_histogram(aes(straightnessIndex),bins=100)


####################################
#### identifying migration days ###
####################################
flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
refIndvL <- lapply(flsReftInd, function(x){
  readRDS(x)
})
refIndvDF <- dplyr::bind_rows(refIndvL) 
head(refIndvDF)

flsDipl <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/", full.names = T)
displL <- lapply(flsDipl, function(x){
  readRDS(x)
})
displDF <- dplyr::bind_rows(displL) 
head(displDF)

refAndDispl <- data.frame(merge(refIndvDF,displDF, by=c("commonID", "individual","date", "locsPerDay")))

rm(refIndvDF)
rm(displDF)

##### trying to find a pattern across all flying sps ###

refAndDisplSUB <- refAndDispl[ refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]

SIcut <- seq(0,1,0.05)

medianLall <- lapply(split(refAndDisplSUB, refAndDisplSUB$MBid), function(mb){
  lapply(SIcut, function(x){
    data.frame(med_mNDkm=median(mb$maxNetDispl_km[mb$straightnessIndex>x],na.rm = T),SIcut=x)
  })
})
dfSImNDall <- dplyr::bind_rows(medianLall)

ggplot(dfSImNDall)+geom_boxplot(aes(y=med_mNDkm, x=SIcut, group=SIcut)) #+ ylim(0,100)

#### looking at specific genus
# Ciconia,Eidolon, Gruidae, Gyps, 
# Aquila, Buteo, Milvus

# genusSel <- "Milvus"
# genusX <- refAndDispl[refAndDispl$genus%in%genusSel & refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 ,] #refAndDispl$tracking_duration_days>730 & refAndDispl$straightnessIndex>0.8 &

## or just selecting randomly from all flying
genusSel <- "random"
genusX <- refAndDispl[refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]

genusXnames <- genusX[!duplicated(paste0(genusX$individual,genusX$MBid)),]
head(genusXnames)

indivNamesMBid <- paste0(genusXnames$MBid,"_",genusXnames$individual)

indivNamesMBidSelect <- sample(indivNamesMBid, 20, replace=F)

minKM <- 50
minSI <- 0.7

pdf(paste0("/home/anne/Documents/GRACEdata/plots/", genusSel,"_",minSI,"_",minKM, ".pdf"), width=11.69, height=8.27) #dinA4
lapply(indivNamesMBidSelect, function(indivSel){
  library("move")
  pthTOindiv <- paste0("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", indivSel,".rds")
  mv <- readRDS(pthTOindiv)     
  
  library(lubridate)
  indiv <- mv@idData$individual.local.identifier
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  tsmv <- data.frame(ts=timestamps(mv), date= floor_date(timestamps(mv), "day"), individual=indiv)
  
  refAndDisplSub <- refAndDispl[refAndDispl$individual== indiv & refAndDispl$MBid==mv@idData$study.id, ]
  
  df <- merge(tsmv, refAndDisplSub, by=c("individual","date"), all.x=T)
  # head(df)
  mv$straightnessIndex <- df$straightnessIndex
  mv$maxNetDispl_km <- df$maxNetDispl_km
  mv$locsPerDay <- df$locsPerDay
  
  dfmv <- as.data.frame(mv)
  # head(dfmv)
  
  
  library("rnaturalearth")
  library("rnaturalearthdata")
  library("ggplot2")
  library("patchwork")
  
  ## plot for sanity check
  world <- ne_countries(scale = "medium", returnclass = "sf")
  p1 <- ggplot(data = world) + geom_sf(color="grey50", fill="grey95") +  
    ggtitle("scale of movement")+
    geom_path(data=dfmv,aes(coords.x1, coords.x2), color="red")+
    coord_sf(xlim = range(dfmv$coords.x1)+c(-1,1), ylim = range(dfmv$coords.x2)+c(-1,1))+ # Limit the map
    ggspatial::annotation_scale(location = 'br')
  
  ## plot maxNetDispl_km ~ straightnessIndex
  SIcut <- seq(0,1,0.05)
  medianL <- lapply(SIcut, function(x){
    median(df$maxNetDispl_km[df$straightnessIndex>x & df$locsPerDay>8],na.rm = T)
  })
  
  medianmND <- unlist(medianL)
  dfSImND <- data.frame(SIcut=SIcut, med_mNDkm=medianmND)
  
  p2 <- ggplot(dfSImND)+geom_point(aes(SIcut,med_mNDkm)) + geom_hline(yintercept = minKM, col="red")+geom_vline(xintercept = minSI, col="firebrick") +ggtitle("maxNetDispl_km ~ straightnessIndex")
  
  
  # summary(dfmv$straightnessIndex)
  p3 <- ggplot(dfmv)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed() + ggtitle("all straightnessIndex")#+ ggspatial::annotation_scale(location = 'br')
  
  dfmv2 <- dfmv
  dfmv2$straightnessIndex[dfmv2$straightnessIndex>minSI &dfmv2$maxNetDispl_km>minKM] <- NA
  dfmv2$straightnessIndex[dfmv2$locsPerDay<7 ] <- NA
  p4 <- ggplot(dfmv2)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed()  + ggtitle(paste0("remove straightnessIndex>",minSI," & maxNetDispl_km>", minKM))#+     ggspatial::annotation_scale(location = 'br')
  
  allnas <- data.frame(table(is.na(dfmv2$straightnessIndex)))
  
  p1 + p2 + p3 + p4 + labs(subtitle = paste0("NAs: ",allnas$Var1[1],": ", allnas$Freq[1] , " & ", allnas$Var1[2],": ", allnas$Freq[2] )) + 
    plot_annotation(title = paste0("sps: ",mv@idData$individual.taxon.canonical.name))
})
dev.off()

## DECISION: 
# minKM <- 50 (to be considered migration day, animal has to travel more then 50km in a straight line (maxNetDispl)) and 
# minSI <- 0.7 (the straightnessIndex has to be over 0.7 to be considered as migration day)
# seem to make sense and work the best

###############################
#### daily UDs calculations ###
###############################

dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs")

library(doParallel)
library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

# source("Functions_For_Movement_Data/dailyUD.R")
source("/home/anne/GIT_SYNC/Grace/Functions_For_Movement_Data/dailyUD.R") # in console

is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", full.names = T)
pathToOutputFolder <- "/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/"

# pathToMV <- files[10]
rasterLayer <- 200 ## resolution in mts of raster
locationError <- 20 ## location error
extExpansionInMts <- 20000 ## expansion in all 4 direction of the raster for dbb calculation
# pathToOutputFolder <- "~/GIT_SYNC/Grace/NoPush/outputTestRuns/"
minLocationsDay <- 8 # min locations required per day
functionDailyMotionVariance <- "mean" # how to sumarize the daily variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
pathToReferenceTables <- "/home/anne/Documents/GRACEdata/ReferenceTables/"
pathTodailyDisplacements <- "/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/"
minKM <- 50 # max maxNetDispl in KM
minSI <- 0.7 # max straightnessIndex
##################

start_time <- Sys.time() 
results <- llply(flsMV,function(x)try({ #x is pathToMV
  dailydBBud(x,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsDay,functionDailyMotionVariance,UDpercentage,pathToReferenceTables,pathTodailyDisplacements,minKM,minSI) 
}),.parallel = T)
saveRDS(results, file=paste0("/home/anne/Documents/GRACEdata/MovementMetrics/","dailyUDcalcs_listWerros",".rds"))

end_time <- Sys.time()
end_time - start_time # 

table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

rL <- results[vapply(results, is.error, logical(1))]

rL1 <- lapply(1:length(rL), function(x) rL[[x]][1])

failed <- pthTOindiv[vapply(results, is.error, logical(1))]

###########################################
## checking how many animals actually got resutls and meking new folder from which Martina can pull
###########################################

flsVar <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyMotionVar_" ) #full.names = T)   
flsUD <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyUDcalc_" ) #full.names = T)
flsDbb <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyDBBcoordinatesSPDF_" ,full.names = T)

dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/dBB_spdf/")

newFolderDbb <- paste0("/home/anne/Documents/GRACEdata/MovementMetrics/dBB_spdf/", list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyDBBcoordinatesSPDF_"))

library(fs)
fs::file_copy(flsDbb, newFolderDbb)

##################################
### merging all tables together for building the model
##################################

flsDipl <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/", full.names = T)
displL <- lapply(flsDipl, function(x){
  readRDS(x)
})
displDF <- dplyr::bind_rows(displL) 
head(displDF)

flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
refIndvL <- lapply(flsReftInd, function(x){
  readRDS(x)
})
refIndvDF <- dplyr::bind_rows(refIndvL) 
head(refIndvDF)
# # saveRDS(refIndvDF, file=paste0("/home/anne/Documents/GRACEdata/refIndvDF_old.rds"))
# 
# refIndvDFold <- readRDS("/home/anne/Documents/GRACEdata/refIndvDF_old.rds")
# colnames(refIndvDFold)[11] <- "locomotionOld"
# 
# ref <- merge(refIndvDF,refIndvDFold, by=c("commonID",     "MBid", "individual",     "species", "genus",       "date", "tracking_duration_days", "GPSpts_total", "locsPerDay", "median_timelag_mins_day"), all=T)
# 
# lost <- ref[is.na(ref$Locomotion),] # snake
# # ref <- ref[complete.cases(ref),]
# diffrent <- ref[!ref$locomotionOld==ref$Locomotion,]
# head(diffrent)
# unique(diffrent$genus)

flsUD <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyUDcalc_" , full.names = T)
UDcalcL <- lapply(flsUD, function(x){
  readRDS(x)
})
UDcalcDF <- data.table::rbindlist(UDcalcL) 
head(UDcalcDF)

flsVar <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyUDcalcs/", pattern = "dailyMotionVar_" , full.names = T)
varL <- lapply(flsVar, function(x){
  readRDS(x)
})
varDF <- data.table::rbindlist(varL) 
head(varDF)

UDcalc <- data.frame(merge(UDcalcDF,varDF, by=c("commonID", "individual","date", "locsPerDay"), all.x=T))
refAndDispl <- data.frame(merge(refIndvDF,displDF, by=c("commonID", "individual","date", "locsPerDay")))
str(UDcalc)
str(refAndDispl)
metricsDF <- merge(refAndDispl,UDcalc,by=c("commonID", "individual","date", "locsPerDay"), all.y=T)

str(metricsDF)
head(metricsDF)

summary(metricsDF)
metricsDF <- metricsDF[(complete.cases(metricsDF$MBid)),]

saveRDS(metricsDF, file=paste0("/home/anne/Documents/GRACEdata/MovementMetrics/","daily_metrics",".rds"))

###########
## checking for trands and patterns in the data
###########

plot(metricsDF$locsPerDay,metricsDF$UDsizeKm2, ylim=c(0,6000))
boxplot(metricsDF$UDsizeKm2~as.factor(metricsDF$locsPerDay), ylim=c(0,300))
hist(metricsDF$UDsizeKm2)
hist(metricsDF$UDsizeKm2, breaks = "FD", xlim=c(0,100))

metricsDFSub <- metricsDF[,c("MBid", "individual","tracking_duration_days","species","genus","Locomotion")]
metricsDFSub <- metricsDFSub[!duplicated(metricsDFSub),]

table(metricsDFSub$Locomotion)

metricsSps <- metricsDFSub[!duplicated(metricsDFSub$species),]
table(metricsSps$Locomotion)

metricsGenus <- metricsDFSub[!duplicated(metricsDFSub$genus),]
table(metricsGenus$Locomotion)

table(metricsDF$Locomotion)


library(ggplot2)
ggplot(metricsDFSub)+ geom_bar(aes(Locomotion))
ggplot(metricsDFSub)+ geom_bar(aes(Locomotion))
