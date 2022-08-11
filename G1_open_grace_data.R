#Script for opening the netcdf data in R and processing them.
#Data was downloaded by Martina
#Aug. 11. 2022. Elham Nourani, PhD. Konstanz, Germany.

library(tidyverse)
library(ncdf4)
library(lubridate)
#library(PCICt) #for calendar conversion

#list files
files <- list.files("/home/enourani/ownCloud/Work/Projects/GRACE/raw_data_from_martina/", pattern = ".nc", full.names = T)

vname <- c("tws", "std_tws", "leakage", "model_atmosphere")

(b <- Sys.time())
lapply(mycl, file_list,function(x){
  
  nc <- nc_open(x)
  
  #extract lon and lat dimensions
  lat <- ncvar_get(nc,'lat') #range: -89.5  89.5
  nlat <- dim(lat) 
  lon <- ncvar_get(nc,'lon') #range: 0.5 359.5
  nlon <- dim(lon) 
  
  #####extract time variable and convert to regular date/time (take into account the calendar)
  t <- ncvar_get(nc, "time")
  nt <- dim(t)
  calendar <- ncatt_get(nc, "time", "calendar")[[2]]
  origin <- strsplit(tunits[[2]], " ")[[1]][3]  
  seconds_per_day <- 60*60*24
  origin_pcict <- as.PCICt(origin, calendar)
  timestamp <- as_datetime(c(t * seconds_per_day), origin = origin)

  #put everything in a large df
  row_names <- expand.grid(lon,lat,timestamp)
  
  var_df <- data.frame(cbind(
    row_names,
    matrix(as.vector(ncvar_get(nc,vname[1])), nrow = nlon * nlat * nt, ncol = 1), #array to vector to matrix
    matrix(as.vector(ncvar_get(nc,vname[2])), nrow = nlon * nlat * nt, ncol = 1),
    matrix(as.vector(ncvar_get(nc,vname[3])), nrow = nlon * nlat * nt, ncol = 1),
    matrix(as.vector(ncvar_get(nc,vname[4])), nrow = nlon * nlat * nt, ncol = 1)))
  
  colnames(var_df) <- c("lon", "lat", "date_time", vname)   #set column names
  
  df <- var_df %>%
    mutate(hour = hour(date_time),
           yday = yday(date_time),
           mnth = month(date_time),
           year = year(date_time)) %>%
    data.frame()
  
  saveRDS(df,file = paste0("/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/grace_", 
                        head(df$year,1), ".rds"))
})

Sys.time() - b #1.8 seconds :p

stopCluster(mycl) 
