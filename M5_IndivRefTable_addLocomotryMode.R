dir.create("/home/anne/Documents/GRACEdata/ReferenceTables/") #only once

library(doParallel)
library(plyr)
mycores <- detectCores()-1 # always make sure to not use all cores of your PC, at least leave 1 free
registerDoParallel(mycores) 

source("Functions_For_Movement_Data/referenceTablesIndividuals.R")

is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", full.names = T)
pathToOutputFolder <- "/home/anne/Documents/GRACEdata/ReferenceTables/"

start_time <- Sys.time()
results <- llply(flsMV, function(f)try({referenceTable_Individuals(f, pathToOutputFolder)})
                 ,.parallel = T)

saveRDS(results, file=paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))

end_time <- Sys.time()
end_time - start_time #17mis

table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

# results <- readRDS(paste0("/home/anne/Documents/GRACEdata/","ReferenceTables_listWerros",".rds"))
# failed <- flsMV[vapply(results, is.error, logical(1))]
# 
# failed[vapply(results, is.error, logical(1))]
