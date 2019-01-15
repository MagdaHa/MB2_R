###############################################
################ visualization ################
#### Author: Steven Hill and Hooman Latifi ####
###############################################
library(RStoolbox)
library(raster)
indir <- "D:\\Excercises\\R\\CHM_Vis_R"
setwd(indir)

#------------------------------------------------------------------------------------------
###visulization DTM CHM (canopy hight model)

cir<-brick("CIR.tif")
dtm<-raster("DTM.tif")
chm<-raster("CHM.tif")
dtm_hillshade<-raster("DTM_hillshade.tif")
chm_hillshade<-raster("CHM_hillshade.tif")

###visulization DTM CHM
#colors
nclr <- 10# number of bins
max <- cellStats(chm,stat='max') 
min <- cellStats(chm,stat='min') #
breaks <- (max - min) / nclr
max.c<-max+breaks
min.c<-min
colfunc <- colorRampPalette(c("beige","chartreuse3","yellow","orange","red"))

#plotting
ggRGB(cir,r = 1, g = 2, b = 3)

#-----------------------------------------------------------------------------------------
###plotting the DTM and CHM
##DTM
x11();  #opens a new window containing afterwards defined plots 
par (mfrow=c(2,2))

#plotting DTM and hillshade
plot(dtm,col=colfunc(100))
plot(dtm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(dtm,col=colfunc(100),alpha=0.55) #DTM with transparency

# now DTM overlaid by Hillshade  (to see the texture better)
plot(dtm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(dtm,col=colfunc(100),alpha=0.55, add=T)  #add: overlays the latest defined plots

#------------------------------------------------
##CHM (cannopy hight model)
x11(); 
par (mfrow=c(2,2))

plot(chm,col=colfunc(20))
plot(chm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(chm,col=colfunc(20),alpha=0.55)

# now CHMS overlaid by hillshade (to see the texture better)
plot(chm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(chm,col=colfunc(20),alpha=0.55,add=T)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# plotting DSM and nDSM

nDSM<- raster("D:\\Excercises\\FUSION\\results\\DSM\\ndsm_transects.asc")
terrain <- raster("D:\\Excercises\\FUSION\\results\\DTM\\terrainmodel_transects_10m.asc")
nDSM
colfunc1 <- colorRampPalette(c("beige","chartreuse3","yellow","orange","red"))
colfunc2 <- colorRampPalette(c("beige","blue","darkblue","darkred","red"))

#-----------------------------------------------
x11();  #opens a new window containing afterwards defined plots 
par (mfrow=c(2,1))
plot(nDSM,legend=F)
plot(terrain,alpha=0.55, add=T)

#or
x11();  #opens a new window containing afterwards defined plots 
par (mfrow=c(2,1))
plot(terrain, col=colfunc1(100), main="terrain model")
plot(nDSM,legend=T, col = colfunc2(100), main="nDSM (Tree hight)")



