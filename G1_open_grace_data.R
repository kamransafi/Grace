#Script for opening the netcdf data in R and saving them as dataframes.
#Data was downloaded by Martina
#Aug. 11. 2022. Elham Nourani, PhD. Konstanz, Germany.

library(tidyverse)
library(ncdf4)
library(lubridate)
library(PCICt) #for calendar conversion

#Open ncdf files and save as dfs
nc_files <- list.files("/home/enourani/ownCloud/Work/Projects/GRACE/raw_data_from_martina/", pattern = ".nc", full.names = T)

vname <- c("tws", "std_tws", "leakage", "model_atmosphere")

df_ls <- lapply(nc_files, function(x){
  
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
  tunits <- ncatt_get(nc, "time", "units")$value
  origin <- strsplit(tunits, " ")[[1]][3]  
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
  
  
  #In years 2011 and 2015, one month has two months' worth of data. correct this here
  
  if(unique(year(var_df$date_time)) == 2011){ #in 2011, it seems like data for November is stored as "2011-10-31 12:00:00 UTC". Convert this to "2011-11-01 12:00:00 UTC"
    var_df[var_df$date_time == as_datetime("2011-10-31 12:00:00 UTC"), "date_time"] <-  as_datetime("2011-11-01 12:00:00 UTC")
  }
 
  if(unique(year(var_df$date_time)) == 2015){ #in 2015, April has two timestamps ("2015-04-16 00:00:00 UTC" and "2015-04-26 15:00:00 UTC"). Should i assume that the second one is for May?? there is no data for May or June
    var_df[var_df$date_time == as_datetime("2015-04-26 15:00:00 UTC"), "date_time"] <-  as_datetime("2015-05-01 15:00:00 UTC")
  }
  
  df <- var_df %>%
    mutate(hour = hour(date_time),
           yday = yday(date_time),
           mnth = month(date_time),
           year = year(date_time)) %>%
    data.frame()  
  
  
  saveRDS(df,file = paste0("/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/grace_", 
                           head(df$year,1), ".rds"))
  
  df
})

#number of months per year
names(df_ls) <- 2002:2021
lapply(df_ls, function(x) unique (x$mnth))

# check to make sure all months have the expected number of rows (i.e. data for two months might be assigned to one)
df_ls %>% 
  reduce(rbind) %>% 
  group_by(year, mnth) %>% 
  summarize(n = n()) %>% 
  filter(n > 64800) #this should return zero rows

