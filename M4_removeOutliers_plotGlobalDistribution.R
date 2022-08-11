#_______________________________________________
## Find and remove outliers based on speed ####

library(move)
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData")

fldOutliers <- "filteringOutliers_examples/" #path to folder to save outlier plots
fldNewMO <- "MoveObjects_1hour_noOutliers/" # path to folder where to save individual data
dir.create(fldOutliers)
dir.create(fldNewMO)

flsMV <- list.files("MoveObjects_1hourSubsample", full.names = T)
studyIds <- sapply(strsplit(flsMV, "/|_"), "[[", 3)
indivIds <- sapply(strsplit(flsMV, "/|_|.rds"), "[[", 4)
fls_studyLs <- split(flsMV, studyIds)
fls_indivLs <- split(indivIds, studyIds)

# Check NAs in coordinates and tag.local.identifier
# results <- lapply(fls_studyLs, function(f){
#   mLs <- lapply(f, readRDS)
#   studyDf <- rbindlist(lapply(lapply(mLs, as.data.frame), "[", , c("study.id","individual.local.identifier","timestamps","coords.x1","coords.x2")))
#   return(anyNA(studyDf$individual.local.identifier))
#   #return(anyNA(studyDf$coords.x1))
#   #return(anyNA(studyDf$study.id))
# })
# table(unlist(results)) 


# Remove outliers based on speed
results <- lapply(fls_studyLs, function(fls)try({
  
  #fls=fls_studyLs[["1088836380"]] #18957668 14671003 1088836380
  mLs <- lapply(fls, readRDS)
  #Assign crs because it is missing in all move objects
  mLs <- lapply(mLs, function(m){
    crs(m) <- CRS("+proj=longlat +ellps=WGS84")
    return(m)})
  #look at speed distribution for the whole study
  grSpeed <- unlist(lapply(mLs, speed))
  qSpeed <- quantile(grSpeed, seq(0,1, 0.0005), na.rm=T)
  #plot(qSpeed)
  # filter each individual based on the last quantile
  mLs_noOut <- lapply(mLs, function(m){
    
    mSub <- m[which(c(NA,speed(m)) <= qSpeed["99.95%"])]
    
    indiv <- unique(m@idData$individual.local.identifier)
    if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
    
    # png(paste0(fldOutliers,unique(m@idData$study.id),"_",indiv,".png"))
    # par(mfrow=c(1,2))
    # plot(m, type="l", ylim=range(coordinates(mSub)[,2]), xlim=range(coordinates(mSub)[,1]))
    # plot(mSub, type="l")
    # dev.off()
    
    #return(mSub)
    if(n.locs(mSub)>0){
      saveRDS(mSub, file=paste0(fldNewMO,unique(mSub@idData$study.id),"_",indiv,".rds"))
    }
  })
}))


## Check reasons for errors:
is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))


#____________________________________________
## Plotting distribution of all studies ####

library(move)
library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(pals) # palette library for class data
library(sf)
library(maps)
world_map <- map_data("world")

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/MoveObjects_1hour_noOutliers")
dir.create("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/distrMaps")

flsMV <- list.files(".")
studyIds <- sapply(strsplit(flsMV, "_"), "[[", 1)
indivIds <- sapply(strsplit(flsMV, "_|.rds"), "[[", 2)
fls_studyLs <- split(flsMV, studyIds)
fls_indivLs <- split(indivIds, studyIds)



# Select a subset of studies to plot
studySub <- fls_studyLs[201:300]

# Create vector of colors, one per MB study
myCols <- rep(as.vector(polychrome(120)), length.out=length(studySub)) # This palette has only 36 colours, for now we just recycle them to reach the right length
# colTab <- data.frame(MBid=names(studySub), col=myCols, 
#                      fakeLong=sample(x=-200:200, length(studySub), replace=T), fakeLat=sample(x=-90:90, length(studySub), replace=T))

# Create base map
baseMap <- ggplot() + 
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill="darkgrey", colour = "black") +
  #geom_point(colTab, mapping = aes(x = fakeLong, y = fakeLat, col=MBid), alpha=0) +
  theme_bw() + xlab("") + ylab("") + ylim(c(-90,90)) + xlim(c(-200,200)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.keys.align=0.5,
        legend.position="bottom", legend.direction="vertical", legend.title.align=0.5,
        legend.title = element_text(size=10), legend.text=element_text(size=8)) #+
  # scale_colour_manual(name="Movebank study IDs", values=colTab$col, drop=T) +
  # guides(col=guide_legend(ncol=18, override.aes = list(size=1.2, alpha=1))) #change point size only in legend

# Add one line per individual and give colors based on study with a label indivicating study.id
for(i in 1:length(studySub)){
  mLs_noOut <- lapply(studySub[[i]], readRDS)
  
  studyDf <- rbindlist(lapply(lapply(mLs_noOut, as.data.frame), "[", , c("study.id","individual.local.identifier","tag.local.identifier","timestamps","coords.x1","coords.x2")))
  studyDf <- studyDf[order(studyDf[,c("individual.local.identifier","timestamps")]),]
  
  baseMap <- baseMap + geom_path(studyDf, mapping = aes(x=coords.x1, y=coords.x2,
                                                        group=interaction(individual.local.identifier, tag.local.identifier)), 
                                 color=myCols[i], alpha=0.6, size=0.7) +
    annotate(geom="label",x=studyDf$coords.x1[1],y=studyDf$coords.x2[1],
             label=names(studySub)[i], fill="white", size = 1.7)
}

baseMap
ggsave("/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/distrMaps/distrMaps_allStudies_pt3_201-300_noOut.tiff", 
       width = 11, height = 7, units = "in", dpi=300)
#saveRDS(baseMap, file="/home/mscacco/ownCloud/Martina/ProgettiVari/GRACE/MovementData/distrMaps/distrMap_pt2_101-250.rds")
