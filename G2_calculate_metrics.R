#Script for calculating useful metrics using grace data
#Data was downloaded by Martina, processed by Elham in G1_open_grace_data.R
#Aug. 19. 2022. Elham Nourani, PhD. Konstanz, Germany.


library(tidyverse)
library(raster)
library(terra)


#extract crs
nc_eg <- raster("/home/enourani/ownCloud/Work/Projects/GRACE/raw_data_from_martina//GRAVIS-3_2015-----------_GFZOP_0600_TWS_GRID_GFZ_0004.nc")
grace_crs <- proj4string(nc_eg)

#open data
dfs <- readRDS( "/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/df_ls.rds") %>% 
  reduce(rbind) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))

#calculate variance of each month across the years
summary_df <- dfs %>% 
  group_by(lat, lon, mnth) %>% 
  mutate(var_tws = var(tws, na.rm = T)) %>%
  ungroup() %>% 
  dplyr::select(c("lon", "lat", "var_tws", "mnth"))

saveRDS(variance_df, file = "/home/enourani/ownCloud/Work/Projects/GRACE/mnth_vars.rds")

#create an r stack with one layer per month
mnth_ls <- split(summary_df, variance_df$mnth)
  
r_stack <- lapply(mnth_ls, function(x){
  r <- rast(x[,-4], crs = grace_crs)
  saveRDS(r, paste0("/home/enourani/ownCloud/Work/Projects/GRACE/grace_vars/grace_var_mnth_", x$mnth[1], ".rds"))
  r
}) %>% 
  rast()

saveRDS(r_stack, file = "/home/enourani/ownCloud/Work/Projects/GRACE/vars_stack.rds")
