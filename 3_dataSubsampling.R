library(data.table)
library(move)
library(plyr)
library(doParallel)
detectCores()
doParallel::registerDoParallel(5)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/Grace_R_GitHub/Data")

fls <- list.files("RawData", full.names = T)

library(lubridate)
ind <- ind[order(ind$timestamp),]
ind <- ind[!duplicated(round_date(ind$timestamp,"5 mins")),]

library(amt)
track_resample(x, rate = hours(2), tolerance = minutes(15), start = 1, ...)