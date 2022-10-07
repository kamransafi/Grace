
library(ncdf4)
library(terra)
library(lubridate)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE")
nc_files <- list.files("RemoteSensingData/GRACE_waterAnomalies_level3/isdcftp.gfz-potsdam.de", pattern = ".nc", full.names = T)

#______________________
## Explore netcdf files
# check the content of one of the files
nc <- nc_open(nc_files[1])
# extract the tws variable (change in total water storage) as brick (one layer per date)
tws <- brick(nc_files[1], varname="tws")
#this contains the time information
tws
tws@z$Date
#extent
extent(tws)
# lat goes from -90 to 90 but long goes from 0 to 360, this needs to be changed fro pacific to atlantic centred view

#____________________________
# Import and format all files

# create a new empty raster with the correct range of lon
atlanticGrace <- raster(nrows=nrow(tws), ncols=ncol(tws), xmn=-180, xmx=180,
                   crs=crs(tws), vals=NA)

# we create a list where each element is a rasterBrick containing all layers of the year, reprojected to atlantic view
atlGrace_br <- brick(unlist(lapply(nc_files, function(nc){
  print(paste0("Processing nc file ",nc))
  tws <- brick(nc, varname="tws")
  pp <- as.data.frame(rasterToPoints(tws))
  pp$x[pp$x>180] <- pp$x[pp$x>180] -360
  coordinates(pp) <- c("x","y")
  crs(pp) <- crs(tws)
  # use the empty atlanticGrace to reproject each layer of the brick to an atlantic view and brick them
  atl_tws <- lapply(names(pp), function(layerTime){
    r <- rasterize(pp, atlanticGrace, field=layerTime, fun=mean, background=NA)
    return(r)
  })
  # re-add the dates of each of the tws layers and store the brick as one element of the list
  names(atl_tws) <- names(pp)
  return(atl_tws)
})))
# assign the date in the z slot of the raster brick and export
atlGrace_br@z$Date <- as.Date(names(atlGrace_br), format="X%Y.%m.%d")
saveRDS(atlGrace_br, file="RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.rds")
#dataType(atlGrace_br)
writeRaster(atlGrace_br, filename="RemoteSensingData/GRACEraw_atlantic_allYearsAllMonths_brick.tif", 
            format="GTiff", datatype="FLT4S", overwrite=TRUE)

#______________________________________
## Format GRACE collection time periods

#Import and format table sent by Eva Boergens with time ranges of data collection for each GRACE layer
tb <- readLines("grace_tws_timestamps.txt")[-1]
tb_df <- as.data.frame(t(sapply(strsplit(sub("[(]", "", sub("[)]", "", tb)), " | - "), "[", c(1,3,4), simplify="vector")))
names(tb_df) <- c("grace_time", "start_collection", "end_collection")
tb_df$grace_time <- as.Date(tb_df$grace_time)
tb_df$start_collection <- as.Date(sub("\\..*", "", tb_df$start_collection), format="%Y%m%d")
tb_df$end_collection <- as.Date(sub("\\..*", "", tb_df$end_collection), format="%Y%m%d")
tb_df$layer_year <- year(tb_df$grace_time)

saveRDS(tb_df, file="RemoteSensingData/grace_tws_collectionPeriods.rds")

