#Script for calculating useful metrics using grace data
#Data was downloaded by Martina, processed by Elham in G1_open_grace_data.R
#Aug. 19. 2022. Elham Nourani, PhD. Konstanz, Germany.


library(tidyverse)
library(raster)
library(terra)
library(ncdf4)


#extract crs
nc_eg <- raster("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/RemoteSensingData/GRACE_waterAnomalies_level3/isdcftp.gfz-potsdam.de//GRAVIS-3_2015-----------_GFZOP_0600_TWS_GRID_GFZ_0004.nc")
#nc_eg <- raster("/home/enourani/ownCloud/Work/Projects/GRACE/raw_data_from_martina//GRAVIS-3_2015-----------_GFZOP_0600_TWS_GRID_GFZ_0004.nc")
grace_crs <- proj4string(nc_eg)

#open data
dfs <- readRDS( "/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/RemoteSensingData/GRACE_waterAnomalies_level3/df_ls.rds") %>%
#dfs <- readRDS( "/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/df_ls.rds") %>% 
  reduce(rbind) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))


#calculate average tws per month across the years
mean_df <- dfs %>% 
  group_by(lat, lon, mnth) %>% 
  mutate(mean_tws = mean(tws, na.rm = T)) %>%
  ungroup() %>% 
  dplyr::select(c("lon", "lat", "mean_tws", "mnth"))

#create an r stack with one layer per month of water availability
mean_mnth_ls <- split(mean_df, mean_df$mnth)

r_stack <- lapply(mean_mnth_ls, function(x){
  r <- rast(x[,-ncol(x)], crs = grace_crs)
  writeRaster(r, paste0("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/RemoteSensingData/GRACE_monthlyMean/grace_meanTWS_mnth_", x$mnth[1], ".tif"), overwrite=TRUE)
  #writeRaster(r, paste0("/home/enourani/ownCloud/Work/Projects/GRACE/grace_means/grace_meanTWS_mnth_", x$mnth[1], ".tif"), overwrite=TRUE)
  r
}) %>% 
  rast()


#calculate variance of each month across the years
variance_df <- dfs %>% 
  group_by(lat, lon, mnth) %>% 
  mutate(var_tws = var(tws, na.rm = T)) %>%
  ungroup() %>% 
  dplyr::select(c("lon", "lat", "var_tws", "mnth"))

#saveRDS(variance_df, file = "/home/enourani/ownCloud/Work/Projects/GRACE/mnth_vars.rds")

#create an r stack with one layer per month of variance
var_mnth_ls <- split(variance_df, variance_df$mnth)
  
var_r_stack <- lapply(var_mnth_ls, function(x){
  r <- rast(x[,-ncol(x)], crs = grace_crs)
  writeRaster(r, paste0("/home/enourani/ownCloud/Work/Projects/GRACE/grace_vars/grace_var_mnth_", x$mnth[1], ".tif"), overwrite=TRUE)
  r
}) %>% 
  rast()


