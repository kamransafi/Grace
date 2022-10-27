###############
## IUCN ## getting the sps lists out of the shp
#############
library(rgdal)
walkMam <- readOGR("/home/anne/Documents/GRACEdata/IUCNshp/WalkingMammals/", "data_0")
head(walkMam)
walkMamSps <- unique(walkMam$BINOMIAL)
saveRDS(walkMamSps, file=paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","WalkingMammals",".rds"))
rm(walkMam)
#----#
swimMam <- readOGR("/home/anne/Documents/GRACEdata/IUCNshp/SwimingMammals/", "data_0")
head(swimMam@data)
swimMamSps <- unique(swimMam$BINOMIAL)
saveRDS(swimMamSps, file=paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","SwimingMammals",".rds"))
rm(swimMam)
#----#
flyMam <- readOGR("/home/anne/Documents/GRACEdata/IUCNshp/FlyingMammals/", "data_0")
head(flyMam@data)
flyMamSps <- unique(flyMam$BINOMIAL)
saveRDS(flyMamSps, file=paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","FlyingMammals",".rds"))
rm(flyMam)
#----#
nonPasser <- readOGR("/home/anne/Documents/GRACEdata/IUCNshp/NonPasserine/", "data_0")
head(nonPasser@data)
nonPasserSps <- unique(nonPasser$BINOMIAL)
saveRDS(nonPasserSps, file=paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","NonPasserine",".rds"))
rm(nonPasser)
#----#
passer <- readOGR("/home/anne/Documents/GRACEdata/IUCNshp/Passerine/", "data_0")
head(passer@data)
passerSps <- unique(passer$BINOMIAL)
saveRDS(passerSps, file=paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","Passerine",".rds"))
rm(passer)

###########################################
### creating individual reference table ### final version of this is at the end of the script ~L150!!!
###########################################

dir.create("/home/anne/Documents/GRACEdata/ReferenceTables/") #only once

library(doParallel)
library(plyr)
mycores <- detectCores()-1 # always make sure to not use all cores of your PC, at least leave 1 free
registerDoParallel(mycores) 

source("Functions_For_Movement_Data/referenceTablesIndividuals.R")

is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", full.names = T)
pathToOutputFolder <- "/home/anne/Documents/GRACEdata/ReferenceTables/"

walkMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","WalkingMammals",".rds"))
swimMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","SwimingMammals",".rds"))
flyMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","FlyingMammals",".rds"))
nonPasserSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","NonPasserine",".rds"))
passerSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","Passerine",".rds"))
walkingGenus <- c(unique(sub(" .*", "", walkMamSps)),"Homo","Chelonoidis","Chelonia","Varanus","Testudinidae","Kinosternon","Taurotragus") #Chelonia
swimmingGenus <- c(unique(sub(" .*", "", swimMamSps)),"Rhincodon","Eretmochelys")
flying <- c(flyMamSps,nonPasserSps,passerSps)
flyingGenus <- c(unique(sub(" .*", "", flying)),"Pandion","Pagophila","Alca","Aves","Cairina","Ichthyaetus","Ardeidae","Anseriformes","Neophron","Gruidae") #Aves,Anseriformes
snakeGenus <- c("Boiga")


start_time <- Sys.time()
results <- llply(flsMV, function(f)try({referenceTable_Individuals(f, pathToOutputFolder,walkingGenus,swimmingGenus,flyingGenus,snakeGenus)})
                 ,.parallel = T)
# results2 <- llply(failed, function(f)try({referenceTable_Individuals(f, pathToOutputFolder,walkingGenus,swimmingGenus,flyingGenus,snakeGenus)})
# ,.parallel = T)


saveRDS(results, file=paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))

end_time <- Sys.time()
end_time - start_time #15mis

table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
flsMV[vapply(results, is.error, logical(1))]
# results <- readRDS(paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))
failed <- flsMV[vapply(results, is.error, logical(1))]

# resultsSPS <- unique(unlist(results[unlist(lapply(results, is.null)==FALSE)])) ## to find out which genus were not included in IUCN


## checking results for the ones with error in 1st round
table(vapply(results2, is.error, logical(1))) # Check potential errors:
names(results2) <- seq_along(results2)
results2[vapply(results2, is.error, logical(1))]
rL <- results2[vapply(results2, is.error, logical(1))]

lapply(1:length(rL), function(x) rL[[x]][1])

failed2 <- failed[vapply(results2, is.error, logical(1))]

#############################################################
### checking table, to evaluate missing info like sps etc ###
#############################################################

flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
refIndvL <- lapply(flsReftInd, function(x){
  readRDS(x)
})
refIndvDF <- data.table::rbindlist(refIndvL)  #dplyr::bind_rows(refIndvL) 
head(refIndvDF)

# refIndvDF[refIndvDF$Locomotion=="unknownSps" & !is.na(refIndvDF$genus),]
length(unique(refIndvDF$MBid[refIndvDF$Locomotion=="unknownSps" & is.na(refIndvDF$genus)]))
missingsps <- data.frame(MBid=unique(refIndvDF$MBid[refIndvDF$Locomotion=="unknownSps" & is.na(refIndvDF$genus) | refIndvDF$genus%in%c("Chelonia", "Aves", "Anseriformes")]))
write.csv(missingsps,file="/home/anne/Documents/GRACEdata/studiesWithMissingSpsName.csv", row.names = F)


sort(table(refIndvDF$genus),decreasing=T)
# Ciconia,Eidolon, Gruidae, Gyps, 
# Aquila, Buteo, Milvus
# remove: "Rhincodon typus, Physeter macrocephalus, Eretmochelys imbricata, Balaenoptera musculus, Balaenoptera physalus, Homo, Animalia, Aves,
# run walking and swiming UDcode

unique(refIndvDF$MBid[refIndvDF$Locomotion=="swimming"])
unique(refIndvDF$species[refIndvDF$Locomotion=="swimming"])

unique(refIndvDF$MBid[refIndvDF$genus=="Animalia" & !is.na(refIndvDF$genus)])

####################################################################
## checking table with manually searched info about sps and genus ##
####################################################################
misSps <- read.csv("/home/anne/Documents/GRACEdata/studiesWithMissingSpsName_MS.csv")
flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", pattern="2082834654", full.names = T)

tbL <- lapply(flsMV, function(x){
  mv <- readRDS(x)
  data.frame(indv=mv@idData$individual.local.identifier, sps=mv@idData$individual.taxon.canonical.name)
})

dplyr::bind_rows(tbL) 


misSpsIncl <- misSps[!misSps$comments%in%c("taxa was available in Movebak","gps calibration data","taxa was available in Movebak (In a first phase the focus will be on the Blackbird, but other thrush species will be included as well)","taxa was available in Movebak, tracking domestic ducks: approach for documenting poultry market chains"),]
misSpsIncl <- misSpsIncl[!misSpsIncl$genus=="",]

write.csv(misSpsIncl, "/home/anne/Documents/GRACEdata/studiesWithMissingSpsName_completed.csv", row.names = F)

misSpsIncl$genus[!misSpsIncl$genus %in% c(walkingGenus,swimmingGenus,flyingGenus)]



###########################################
### creating individual reference table ### final version!!!
###########################################

dir.create("/home/anne/Documents/GRACEdata/ReferenceTables/") #only once

library(doParallel)
library(plyr)
mycores <- detectCores()-1 # always make sure to not use all cores of your PC, at least leave 1 free
registerDoParallel(mycores) 

source("Functions_For_Movement_Data/referenceTablesIndividuals.R")

is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", full.names = T)
pathToOutputFolder <- "/home/anne/Documents/GRACEdata/ReferenceTables/"

walkMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","WalkingMammals",".rds"))
swimMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","SwimingMammals",".rds"))
flyMamSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","FlyingMammals",".rds"))
nonPasserSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","NonPasserine",".rds"))
passerSps <- readRDS(paste0("/home/anne/Documents/GRACEdata/IUCNshp/SpsRDS/","Passerine",".rds"))
walkingGenus <- c(unique(sub(" .*", "", walkMamSps)),"Homo","Chelonoidis","Chelonia","Varanus","Testudinidae","Kinosternon","Taurotragus","Terrapene") #Chelonia
swimmingGenus <- c(unique(sub(" .*", "", swimMamSps)),"Rhincodon","Eretmochelys")
flying <- c(flyMamSps,nonPasserSps,passerSps)
flyingGenus <- c(unique(sub(" .*", "", flying)),"Pandion","Pagophila","Alca","Aves","Cairina","Ichthyaetus","Ardeidae","Anseriformes","Neophron","Gruidae") #Aves,Anseriformes
snakeGenus <- c("Boiga")

missSps <- read.csv("/home/anne/Documents/GRACEdata/studiesWithMissingSpsName_completed.csv")


start_time <- Sys.time()
results <- llply(flsMV, function(f)try({referenceTable_Individuals(f, pathToOutputFolder,walkingGenus,swimmingGenus,flyingGenus,snakeGenus,missSps)})
                 ,.parallel = T)

saveRDS(results, file=paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))

end_time <- Sys.time()
end_time - start_time #15mis

table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
flsMV[vapply(results, is.error, logical(1))]
# results <- readRDS(paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))
failed <- flsMV[vapply(results, is.error, logical(1))]