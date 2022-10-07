### to get the weighted daily GRACE per indiv, 
# 1- annotate the "dailyDBBcoordinatesSPDF_.......rds" with grace, 
# 2- than multiply the extracted grace value by the column "layer" (dbb_val), 
# 3- and sum it up per day -> this value is the weighted mean grace per day

commonID=paste(MBid, indiv, date, sep="_")

columns of DF: commonID, date, layer(rename to dbb_val), grace

multiply dbb_val * grace

sum that per date