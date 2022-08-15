setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")
dir.create("MovementData/MovementMetrics")

source("Grace_R_GitHub/Functions_For_Movement_Data/cumulativeDistance.R")
source("Grace_R_GitHub/Functions_For_Movement_Data/maxNetDisplacement.R")
is.error <- function(x) inherits(x, "try-error")

flsMV <- list.files("MovementData/MoveObjects_1hour_noOutliers", full.names = T)


#_________________________________________________
## Calculate daily cumulative distance in km ####

outputFolder <- "MovementData/MovementMetrics/cumulativeDistance/"
dir.create(outputFolder)

results <- lapply(flsMV, function(f)try({cumulativeDist(f, outputFolder)}))

# Check potential errors:
table(vapply(results, is.error, logical(1)))

#________________________________________________
## Calculate daily maximum net displacement ####

outputFolder <- "MovementData/MovementMetrics/maxNetDisplacement/"
dir.create(outputFolder)

results <- lapply(flsMV, function(f)try({maxNetDisp(f, outputFolder)}))

# Check potential errors:
table(vapply(results, is.error, logical(1)))

