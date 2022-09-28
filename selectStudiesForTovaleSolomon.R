
# For Tovale Solomon: studies of avian species in TeamWikelski MB account that contain 30 individuals for 2 weeks to 3 months
df <- read.csv("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/studies_summaryTable_downloadOrNot_duplicateOrNot.csv")

table(df$studyDownloaded)
df <- df[grep("yes", df$studyDownloaded),]

table(df$studyDuplicated)
anyNA(df$studyDuplicated)
df <- df[which(df$studyDuplicated == "no"),]

# we decided not to filter by number of individuals
#range(df$number_of_individuals)
#df <- df[which(df$number_of_individuals > 20),]

# Use movebank to download the species name and the tracking duration for the studies selected above
library(move)
source("/home/mscacco/ownCloud/Martina/PHD/R_functions/IdentifyAvianSpecies.R")

creds <- movebankLogin()

df[,c("species","minTrackingDuration_weeks","maxTrackingDuration_weeks","timestamp_start","timestamp_end")] <- NA
for(i in (1:nrow(df))[-c(6,614)]){ # error for i=6
  print(i)
  tb <- getMovebankAnimals(study=df$id[i], creds) 
  df$species[i] <- paste(unique(tb$taxon_canonical_name), collapse="|")
  tb$timestamp_start <- as.POSIXct(tb$timestamp_start, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  tb$timestamp_end <- as.POSIXct(tb$timestamp_end, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  indTrackDuration <- round(as.numeric(difftime(tb$timestamp_end, tb$timestamp_start, units = "weeks")),2)
  df$timestamp_start[i] <- as.character(min(tb$timestamp_start, na.rm=T))
  df$timestamp_end[i] <- as.character(min(tb$timestamp_end, na.rm=T))
  df$minTrackingDuration_weeks[i] <- min(indTrackDuration, na.rm=T)
  df$maxTrackingDuration_weeks[i] <- max(indTrackDuration, na.rm=T)
}

write.csv(df, "/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/subStudiesWikelski_TovaleSolomon.csv", row.names = F)

# select only bird species, with a minimum tracking duration of 2 weeks
dfBirds <- df[which.is.bird(df$species),]
dfBirds <- dfBirds[which(dfBirds$maxTrackingDuration >= 2),]

write.csv(dfBirds, "/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/subStudiesWikelski_TovaleSolomon_birds_2weeks.csv", row.names = F)


# indDf <- readRDS("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/referenceTableStudies_ALL_excludedColumn.rds")
# indDf <- indDf[which(indDf$excluded=="no"),]
# indDf_birds <- indDf[which.is.bird(indDf$species),]
# length(unique(indDf_birds$MBid))




