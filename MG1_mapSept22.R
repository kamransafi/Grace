
library(raster)
library(rasterVis)
library(viridis)
library(mapview)
library(sf)
library(rgdal)
library(data.table)
library(RStoolbox)
library(ggplot2)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/")
fls_var <- list.files("./RemoteSensingData/GRACE_monthlyVariance", pattern="tif", full.names = T)
fls_avg <- list.files("./RemoteSensingData/GRACE_monthlyMean", pattern="tif", full.names = T)
# mean of the monthly variance across the 20 years
graceVar <- mean(stack(lapply(fls_var, raster)))
graceAvg <- mean(stack(lapply(fls_avg, raster)))
grace <- stack(graceAvg, graceVar)
names(grace) <- c("graceAvg", "graceVar")
head(coordinates(grace))
tail(coordinates(grace))
levelplot(grace)

# Change from pacific to atlantic centered view
newGrace <- raster(nrows=nrow(grace), ncols=ncol(grace), xmn=-180, xmx=180,
                   crs=crs(grace), vals=NA)

pp <- as.data.frame(rasterToPoints(grace))
pp$x[pp$x>180] <- pp$x[pp$x>180] -360
coordinates(pp) <- c("x","y")
crs(pp) <- crs("+init=epsg:4326")
plot(pp)
newGraceAvg <- rasterize(pp, newGrace, field="graceAvg", fun=mean, background=NA)
newGraceVar <- rasterize(pp, newGrace, field="graceVar", fun=mean, background=NA)
newGrace <- stack(newGraceAvg, newGraceVar)
names(newGrace) <- c("mean_graceAvg", "mean_graceVar")
saveRDS(newGrace, file="./RemoteSensingData/grace_meanOfMonthlyAvgVar.rds")
levelplot(newGrace)

# crop to the extent of where data are available
newGrace <- crop(newGrace, extent(pp))
# reproject to global metric crs for mapping
#new_crs <- st_crs(3395)
grace_proj <- projectRaster(newGrace, crs = crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs
")) #new_crs$wkt)
levelplot(grace_proj)
plot(grace_proj)

# import, crop, aggregate and reproject background maps
dark <- brick("RemoteSensingData/backgroundMaps/GRAY_50M_SR_OB/GRAY_50M_SR_OB.tif")
nat <- brick("RemoteSensingData/backgroundMaps/HYP_50M_SR_W/HYP_50M_SR_W.tif")
dark <- aggregate(dark, 10)
nat <- aggregate(nat, 10)
darkP <- projectRaster(dark, crs = crs(grace_proj))
natP <- projectRaster(nat, crs = crs(grace_proj))
darkCrop <- crop(darkP, extent(grace_proj))
natCrop <- crop(natP, extent(grace_proj))
ggR(darkCrop, geom_raster = F) #use geom_raster=F to plot as image and not as raster
ggRGB(natCrop, r=1, g=2, b=3)

# different background map
hs <- brick("RemoteSensingData/backgroundMaps/PRIMSA_SR_50M.tif")
land <- readOGR("RemoteSensingData/backgroundMaps/ne_50m_landPolygons/ne_50m_land.shp")
hs_mask <- mask(hs, land)
oc <- brick("RemoteSensingData/backgroundMaps/OB_50M.tif")
plot(extent(-180,180,-90,90), axes=F, xlab="", ylab="", asp=1) #bty="n"
plotRGB(oc, add=T)
plot(hs_mask, useRaster=F, col=gray.colors(7), add=T, legend=F) #use geom_raster=F to plot as image and not as raster
plot(stretch(newGrace[["mean_graceVar"]], maxq=.95), add=T, col=viridis(10), legend=F, alpha=.65)


# the extents don't match by few degrees for some reason, we crop it as it's only for visualization purposes
grace_projC <- crop(grace_proj, extent(natCrop))

# final raster map
#ggR(darkCrop, geom_raster = F) + #or
baseMap <- ggRGB(natCrop, r=1, g=2, b=3) +  
  ggR(grace_projC[["mean_graceVar"]], maxpixels =  1624980, geom_raster = T, stretch = "hist", alpha=.7, ggLayer = T) + # stretch either 'none', 'lin', 'hist', 'sqrt' or 'log'
  coord_sf() + 
  scale_fill_viridis(name="Monthly \nvariance", na.value = NA) +
  theme_bw() + xlab("")+ ylab("") +
  theme(axis.line = element_blank(), #element_line(colour = "black", size=.2),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
baseMap

#mapview(grace_proj, maxpixels =  1624980, na.col="transparent", map.types = "Esri.WorldShadedRelief")

# Import movement data to add to the map
indFls <- list.files("MovementData/MoveObjects_1hour_noOutliers", full.names=T)

# find studies at longitudinal margins
# for(f in indFls[800:900]){
#   mv<-readRDS(f)
#   print(paste(f, paste(range(coordinates(mv)),collapse=";"), sep=" - "))
# }

#for testing removal of horizontal lines
#mv <- data.frame(x=c(-180,-175,-180,180,175,-180,-160), y=rep(0,7), time=seq.Date(as.Date(Sys.time()), by=1, length.out=7))
#mv <- move(mv$x, mv$y, time=as.POSIXct(mv$time, format="%Y-%m-%d"), data=mv, proj = crs("+proj=longlat +ellps=WGS84"))
#fls=grep("1091848505",indFls,  value=T)

for(f in indFls){
#for(f in fls){
  mv <- readRDS(f)
  mvP <- spTransform(mv, CRSobj = crs(grace_proj))
  if(ymax(mvP) < ymax(grace_projC) & ymin(mvP) > ymin(grace_projC)){ #this is to exclude move obj that fall outside the gra product boundaries
    
    if(xmin(mv) < -90 & xmax(mv) > 90){ # this is to correct for horizontal lines, animals at the margin gets split in two (negative and positive coords)
      plotGroup <- rep(1, n.locs(mvP))
      plotGroup[coordinates(mvP)[,1]>0] <- 2
      plotGroup <- c(0, cumsum(plotGroup[-1] - plotGroup[-length(plotGroup)] != 0))
      
      baseMap <- baseMap +
        geom_path(data=as.data.frame(mvP), aes(x=coords.x1, y=coords.x2, group=plotGroup), alpha=.3, size=.6, col="black")
      
    }else{
    #plot(mv[plotGroup==0], type="l")
     #baseMap <- baseMap +
    #geom_points(data=as.data.frame(mvP), aes(x=coords.x1, y=coords.x2), col="black", alpha=.5, size=.3)
    baseMap <- baseMap +
      geom_path(data=as.data.frame(mvP), aes(x=coords.x1, y=coords.x2), alpha=.3, size=.6, col="black")
    }
  }
}
x11();baseMap

save(newGrace, oc, hs_mask, nat, file="./FirstMap_Sept22/rastersForBackgroundMap.rdata")

#____________________
# With graphics ####
#____________________
library(scales)
library(raster)
library(move)
library(viridis)
# library(doParallel)
# library(plyr)
# registerDoParallel(5)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/")
load("./FirstMap_Sept22/rastersForBackgroundMap.rdata")
indFls <- list.files("MovementData/MoveObjects_1hour_noOutliers", full.names=T)

refTab <- readRDS("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/referenceTableStudies_ALL_excludedColumn.rds")
refTab <- refTab[which(refTab$excluded=="no"),]
length(unique(refTab$species))
length(unique(refTab$MBid))


compareCRS(mv, newGrace)

#___________________________________
# Single palette for grace variance
#____________________________________

myPal <- colorRampPalette(colors = c("darkmagenta","firebrick4","firebrick","darkorange","yellow","bisque","cadetblue1","darkblue"))(100)

jpeg("./FirstMap_Sept22/final/nat_changeTWS_points_black_alpha01.jpeg", width=120, height=120/2, units="cm", res=500)
par(mar=c(3,3,3,3))
plot(extent(-180,180,-90,90), axes=F, xlab="", ylab="", asp=1) #bty="n"
plotRGB(nat, add=T)
# plotRGB(oc, add=T)
# plot(hs_mask, useRaster=F, col=gray.colors(7), add=T, legend=F) #use geom_raster=F to plot as image and not as raster
# plot(stretch(newGrace[["mean_graceVar"]], maxq=.95), add=T, col=magma(15), legend=F, alpha=.65)
plot(newGrace[["mean_graceAvg"]], add=T, col=myPal, legend=F, alpha=.55)
plot(newGrace[["mean_graceAvg"]], legend.only=TRUE, col=myPal,
     smallplot=c(0.96,0.97, 0.35,0.65),
     # legend.width=2,
     axis.args=list(cex.axis=2),
     legend.args=list(text='Change in total water storage (cm)', side=2, font=2, line=1, cex=2)) #side=4, cex=0.8, line=2.5 for right side legend
for(f in indFls){
  mv <- readRDS(f)
 #points(mv, col=alpha("black",.4), cex=.07, pch=19)
  #points(mv, col=alpha("white",.2), cex=.07, pch=19)
  points(mv, col=alpha("black",.1), cex=.07, pch=19)

  # if(ymax(mv) < ymax(newGrace) & ymin(mv) > ymin(newGrace)){ #this is to exclude move obj that fall outside the gra product boundaries
  #   
  #   if(xmin(mv) < -90 & xmax(mv) > 90){ # this is to correct for horizontal lines, animals at the margin gets split in two (negative and positive coords)
  #     plotGroup <- rep(1, n.locs(mv))
  #     plotGroup[coordinates(mv)[,1]>0] <- 2
  #     plotGroup <- c(0, cumsum(plotGroup[-1] - plotGroup[-length(plotGroup)] != 0))
  #     
  #   }else{plotGroup <- 1}
  #   
  #   mv_ls <- split(mv, plotGroup)
  #   lapply(mv_ls, lines, col=alpha("black",.5), lwd=1.5)
  # }
}
dev.off()


#____________________________________________________________
# Double color palette for grace variance and absolute value
#____________________________________________________________

library(raster)
library(scales)
library(gtools)
library(move)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/")

# import newGrace
newGrace <- readRDS(file="./RemoteSensingData/grace_meanOfMonthlyAvgVar.rds")
# import background maps
load("./FirstMap_Sept22/rastersForBackgroundMap.rdata")
# import individuals
indFls <- list.files("MovementData/MoveObjects_1hour_noOutliers", full.names=T)

#----------------------------------------
# Define 2D color palette (Kami's script)

library(classInt)
my.data<-seq(0,1,.01)

my.class<-classIntervals(my.data,n=10,style="quantile")
# original palette from kami
# my.pal.1<-findColours(my.class,c(rgb(0,150,235, maxColorValue=255),"grey")) #original from kami
# my.pal.2<-findColours(my.class,c(rgb(130,0,80, maxColorValue=255), rgb(255,230,15, maxColorValue=255))) 
# modified
my.pal.1<-findColours(my.class,c("darkgreen","goldenrod1")) #high to low
my.pal.2<-findColours(my.class,c("darkmagenta","cyan")) #high to low

plot(rep(0,101),my.data,pch=19,col=my.pal.1, cex=1, xlim=c(0,1),ylim=c(0,1))
points(rep(1,101),my.data,pch=19,col=my.pal.2, cex=1)

# blue, greem   
#red,yellow  
#red:
#green: rgb(17,255,20, maxColorValue=255))                    

# loop: use left and right vertical color ramp & interpolate horizontally
col.matrix <- matrix(nrow = 101, ncol = 101, NA)
for(i in 1:101){
  my.col <- c(paste(my.pal.1[i]),paste(my.pal.2[i])) # choose colors
  col.matrix[102-i,] <- findColours(my.class,my.col)
}

#-----------------
# plot full grid

# plot(rep(0,101),my.data,pch=19,col=my.pal.1, cex=0.5,
#      xlim=c(0,1),ylim=c(0,1))
jpeg("./FirstMap_Sept22/final/2D_legend_M.jpeg", width=20, height=20, units="cm", res=300)
#png("./FirstMap_Sept22/2D_legend_M_alpha.png", width=20, height=20, units="cm", res=300, bg="transparent")
png("./FirstMap_Sept22/final/2D_legend_M_yellowGreenMagentaCyan_noAxes_alpha.png", width=20, height=20, units="cm", res=300, bg="transparent")
#par(mar=c(6,6,3,3))
par(mar=c(3,3,3,3))
plot(rep(0,101),my.data,pch=19,col=my.pal.1, cex=0.5,
     xlim=c(0,1),ylim=c(0,1),
     #main="Change in terrestrial total water storage\n averaged 2002-2021",
     axes=F,
     xlab="", 
     ylab="")
# title(xlab="Average monthly var(change tws) 2002-2021\n(min=3.6, med=21.4, max=6063.0)", line=3.5, cex.lab=1.1, font=2)
# title(ylab="Average monthly change in tws 2002-2021\n(min=-16.7, med=-0.2, max=5.2)", line=2.5, cex.lab=1.1, font=2)
for(i in 1:101){
  col.temp<-col.matrix[i-1,]
  points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
}
dev.off()
col.matrix <- col.matrix[c(1,10,20,30,40,50,60,70,80,90,100), c(1,10,20,30,40,50,60,70,80,90,100)]
matrix <- col.matrix

#----------------------------------------
# Associate color matrix to raster stack

rDf <- rasterToPoints(newGrace, spatial=T)

rDf$qAvg <- as.numeric(quantcut(rDf@data$mean_graceAvg, q = seq(0,1,0.1)))
rDf$qVar <- as.numeric(quantcut(rDf@data$mean_graceVar, q = seq(0,1,0.1)))
summary(rDf@data$mean_graceAvg)
summary(rDf@data$mean_graceVar)

colVect <- as.vector(t(matrix))
#index in the color vector
rDf$colorIndex <- rDf$qAvg * rDf$qVar
rDf$colorName <- as.factor(colVect[rDf$colorIndex])
# q1*q2 as vector(position in matrix = to index of class of 1 column and other column)
rCol <- rasterize(rDf, newGrace, field="colorIndex", fun=mean, background=NA)
plot(rCol, col=colorRampPalette(colVect)(121), axes=F, legend=F, box=F)

#--------------------------
# Plot trajectories on top

jpeg("./FirstMap_Sept22/final/nat_2DpaletteM_yellowGreenMagentaCyan_points_black_alpha01.jpeg", width=120, height=120/2, units="cm", res=500)
plot(extent(-180,180,-90,90), axes=F, xlab="", ylab="", asp=1) #bty="n"
plotRGB(nat, add=T)
plot(rCol, col=colorRampPalette(colVect)(121), add=T, legend=F, alpha=.65)

for(f in indFls){
  mv <- readRDS(f)
    points(mv, col=alpha("black",.1), cex=.07, pch=19)
  }
dev.off()



