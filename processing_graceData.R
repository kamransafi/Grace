#__________________
## GRACE DATA ####

library(ncdf4)
library(raster)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/RemoteSensingData/GRACE_waterAnomalies_level3/isdcftp.gfz-potsdam.de")

#unzip("/home/mscacco/Martina_noBackedUp/windData_myles_moths/weather_meteoswiss_2019.zip", exdir="/home/mscacco/Martina_noBackedUp/windData_myles_moths/weatherData_2019")

ncs <- list.files(".", full.names=T)

# extract altitude layers height in metres (info is in the z_1 slot)
ncOpen <- nc_open(ncs[3])
z <- ncvar_get(ncOpen, varid = "tws")

# lat <- raster(ncs[3], varname="lat"); plat <- rasterToPoints(lat) #first extract projection and extent
# lon <- raster(ncs[3], varname="lon"); plon <- rasterToPoints(lon)
# lonlat <- cbind(plon[,3], plat[,3]); lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

r <- stack(ncs[3])


LayerTimes <-  sapply(lapply(ncs, raster, varname="FF", lvar="z_1", level=1, crs=crs(lonlat)), function(x)x@z[[1]])
LayerTimes_dots <- gsub("[[:punct:]]| ", ".", LayerTimes)
LayersDates <- substr(LayerTimes_dots, 1,10)
