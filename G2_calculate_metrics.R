#Script for calculating useful metrics using grace data
#Data was downloaded by Martina, processed by Elham in G1_open_grace_data.R
#Aug. 19. 2022. Elham Nourani, PhD. Konstanz, Germany.




#bioclim requires average monthly values as well and min and max of the month (https://www.worldclim.org/data/bioclim.html). 
#we only have one average value per month. but we could do quarterly

#open data

#files <- list.files("/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/", pattern = "rds", full.names = T)

dfs <- readRDS( "/home/enourani/ownCloud/Work/Projects/GRACE/data_dfs/df_ls.rds") %>% 
  reduce(rbind)


annual_vars_df <- dfs %>%
  group_by(lat, lon, year) %>% #for each location, for each year, calculate the following:
  summarize(annual_mean_tws = mean(tws, na.rm = T),
            annual_range_tws = max(tws, na.rm = T) - min(tws, na.rm = T),
            seasonality_tws = sd(tws,na.rm = T) * 100, #bioclim multiplies this by 100, so I did too
            annual_max_tws = max(tws, na.rm = T), #tws of the wettest month
            annual_min_tws = min(tws, na.rm = T), #tws of the driest month
            wettest_mnth = mnth[which.min(tws)],
            driest_mnth = mnth[which.max(tws)]
 ) 

#estimate quarterly vars and merge with annual ones
quarter_vars_df <- dfs %>% 
  mutate(quarter = ifelse(between(mnth, 1,3), "first", 
                          ifelse(between(mnth, 3,6), "second",
                                 ifelse(between(mnth, 7,9), "third", "fourth")))) %>% #create variable for quarter
  group_by(lat, lon, year, quarter) %>% #for each location, for each quarter of each year, calculate the following:
  mutate(quarter_mean_tws = mean(tws, na.rm = T),
         quarter_min_tws = min(tws, na.rm = T),
         quarter_max_tws = max(tws, na.rm = T))
  
  all_vars <- dfs %>% 
  group_by(lat, lon, year, quarter) %>% 
    summarize(quarter_min_tws = )
    
  
#calculate variables inspired by WorldClim (https://www.worldclim.org/data/bioclim.html)

#annual trends: mean annual tws


#seasonality: annual range in tws 

#extreme or limiting factors: min tws of the driest month, max tws of the wettest month, 
