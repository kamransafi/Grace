
# Bring in the table with the calculated metrics per day, merge the information and average them by the grace collection periods
# Use this to build a dataset and run the following models:
#_______________
# 1- monthly average of daily UD size ~ monthly grace experienced (calculated by multiplying UD probabilities * monthly grace values) - we expect larger UDs in poorer conditions
# 2- monthly average of daily cumulative distance ~ monthly grace experienced (calculated as above) - higher daily distance in poorer conditions
# 3- monthly "directness" of movement within a pixel ~ monthly grace raw values per pixel (pixel based, we could calculate directness by averaging the DBB variance per segment by month and pixel) - we expect more directed movement (lower dbb variance) in poor quality pixels
#_______________

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")

# Import table with the GRACE collection periods
graceTimes <- readRDS("RemoteSensingData/grace_tws_collectionPeriods.rds")
graceTimes <- graceTimes[order(graceTimes$grace_time),]
# Import table with metrics and grace experienced per day and per individual
daily_graceExp <- readRDS("MovementData/UDs/nonFlying_allDailyGraceExperienced.rds")
daily_metrics <- readRDS("MovementData/UDs/nonFlying_UDcalculations.rds")

