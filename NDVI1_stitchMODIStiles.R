
## MODIS data "MODIS/Terra Vegetation Indices 16-Day L3 Global 1km SIN Grid V061" downloaded by Elham
## Script by Martina, March 31 2023

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/RemoteSensingData/MODIS")

# in the file name, date is specified as julian day between the first and the second "." in the file name
fls <- unzip("Modis_Feb2020.zip", list=T) #list files without extracting
jday <- sapply(strsplit(grep("hdf", fls$Name, value=T), ".", fixed=T), "[", 2)
table(jday)
tile <- sapply(strsplit(grep("hdf", fls$Name, value=T), ".", fixed=T), "[", 3)
table(tile)

# extract files for one day
tmpFold <- tempdir()
oneDay <- unique(jday)[2]
asDate <- as.Date(as.numeric(sub("A2020","",oneDay)), origin = as.Date("2020-01-01"))
unzip("Modis_Feb2020.zip", files = grep(oneDay, fls$Name, value=T), exdir = tmpFold)

modFls <- list.files(paste0(tmpFold,"/Modis_Feb2020"), full.names = T)

library(terra)
#describe file content to only extract NDVI (layer 1)
meta <- describe(modFls[1], sds=T)
# vegetacion indeces: valid range -2000, 10000 | scale factor for integer 0.0001
ndviID <- meta$id[meta$var == grep("NDVI", meta$var, value=T)] 
eviID <- meta$id[meta$var == grep("EVI", meta$var, value=T)]
# quality assessment
# 00 VI produced with good quality #include
# 01 VI produced, but check other QA #include
# 10 Pixel produced, but most probably cloudy #exclude
# 11 Pixel not produced due to other reasons than clouds #exclude

qualityId <- meta$id[meta$var == grep("VI Quality", meta$var, value=T)] #??
relId <- meta$id[meta$var == grep("reliability", meta$var, value=T)] #keep only 00 and 01?

#r <- rast(f, lyrs=ndviID)
ndviLs <- lapply(modFls, rast, lyrs=ndviID)
eviLs <- lapply(modFls, rast, lyrs=eviID)
qualLs <- lapply(modFls, rast, lyrs=qualityId)
relLs <- lapply(modFls, rast, lyrs=relId)
hist(values(ndviLs[[1]]))
hist(values(qualLs[[1]]))
  
ndvi <- do.call(merge, ndviLs)
evi <- do.call(merge, eviLs)
pxRel <- do.call(merge, relLs)
plot(ndvi)
plot(pxRel)

