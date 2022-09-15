
library(raster)
library(rasterVis)
library(viridis)
library(mapview)
library(sf)
library(data.table)
library(RStoolbox)
library(ggplot2)

fls <- list.files("/home/martina/Desktop/GRACE/", pattern="tif", full.names = T)
# mean of the monthly variance across the 20 years
grace <- mean(stack(lapply(fls, raster)))
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
newGrace <- rasterize(pp, newGrace, field="layer", fun=mean, background=NA)
levelplot(newGrace)

# crop to the extent of where data are available
newGrace <- crop(newGrace, extent(pp))
# reproject to global metric crs for mapping
new_crs <- st_crs(3395)
grace_proj <- projectRaster(newGrace, crs = new_crs$wkt)
levelplot(grace_proj)
plot(grace_proj)

# import, crop, aggregate and reproject background maps
dark <- brick("/home/martina/Desktop/GRACE/backgroundMaps/GRAY_50M_SR_OB/GRAY_50M_SR_OB.tif")
nat <- brick("/home/martina/Desktop/GRACE/backgroundMaps/HYP_50M_SR_W/HYP_50M_SR_W.tif")
dark <- aggregate(dark, 10)
nat <- aggregate(nat, 10)
darkP <- projectRaster(dark, crs = crs(grace_proj))
natP <- projectRaster(nat, crs = crs(grace_proj))
darkCrop <- crop(darkP, extent(grace_proj))
natCrop <- crop(natP, extent(grace_proj))
ggR(darkCrop, geom_raster = F) #use geom_raster=F to plot as image and not as raster
ggRGB(natCrop, r=1, g=2, b=3)

# the extents don't match by few degrees for some reason, we crop it as it's only for visualization purposes
grace_projC <- crop(grace_proj, extent(natCrop))

# final raster map
#ggR(darkCrop, geom_raster = F) + #or
ggRGB(natCrop, r=1, g=2, b=3) +  
ggR(grace_projC, maxpixels =  1624980, geom_raster = T, stretch = "hist", alpha=.7, ggLayer = T) + # stretch either 'none', 'lin', 'hist', 'sqrt' or 'log'
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

#mapview(grace_proj, maxpixels =  1624980, na.col="transparent", map.types = "Esri.WorldShadedRelief")




