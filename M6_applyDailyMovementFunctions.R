# dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/") #only once

# dir.create("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements")

library(doParallel)
library(plyr)
mycores <- detectCores()-1 # always make sure to not use all cores of your PC, at least leave 1 free
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
end_time - start_time # 1.7h without paralleizing

table(vapply(results, is.error, logical(1))) # Check potential errors:

##########
## some exploratory plots 
#####
library(ggplot2)

flsDipl <- list.files("/home/anne/Documents/GRACEdata/MovementMetrics/dailyDisplacements/", full.names = T)

randFls <- sample(flsDipl, 1000, replace=F)

randL <- lapply(randFls, function(x){
# randL <- lapply(flsDipl, function(x){ 
  readRDS(x)
})

# randDF <- do.call("rbind",randL)
randDF <- dplyr::bind_rows(randL) 

# head(randDF)

hist(randDF$locsPerDay)
# hist(randDF$straightnessIndex)#, breaks="FD")
# plot(randDF$cumulativeDist_km~randDF$straightnessIndex)

ggplot(randDF[randDF$locsPerDay>=8,])+geom_histogram(aes(straightnessIndex),bins=100)

ggplot(randDF[randDF$straightnessIndex>=0.5 & randDF$locsPerDay>=8,])+geom_histogram(aes(cumulativeDist_km),bins=100)+xlim(0,500)
