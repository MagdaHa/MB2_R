############################################################################
####examples of working with airborne point clouds inside rLiDAR library####
###############LiDAR Data Processing and Visualization######################
######Set of tools for reading, processing and visualizing small set of#####
#LiDAR (Light Detection and Ranging) data for forest inventory applications#
############################################################################

#load the package
library (rLiDAR)
##################################################################################################

# examples for LidR package
##################################################################################################
##################################################################################################
# function to read  LAS file

#=======================================================================#
# Importing LAS file:
#=======================================================================#
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
# Reading LAS file
rLAS<-readLAS(LASfile,short=TRUE)
# Summary of the LAS file
summary(rLAS)
#=======================================================================#
# LAS file visualization:
#=======================================================================#
# 01 Set a single color
col<-"forestgreen"

# plot 2D
plot(rLAS[,1],rLAS[,2], col=col,xlab="UTM.Easting", ylab="UTM.Northing", main="Single color")

# plot 3D
library(rgl)
points3d(rLAS[,1:3], col=col, axes=FALSE,xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-")) # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height(m)", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # terrain

# 02 Set a color by height
# color ramp

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Color by height
col <- myColorRamp(c("blue","green","yellow","red"),rLAS[,3])

# plot 2D
plot(rLAS[,1], rLAS[,2], col=col, xlab="UTM.Easting", ylab="UTM.Northing", main="Color by height")

# plot 3D
points3d(rLAS[,1:3], col=col, axes=FALSE, xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-")) # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height(m)", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray",alpha=0.7) # terrain

###################################################################################################
# function for 3D forest stand visualization
#load an examplified canopy height model (CHM)

data(chm)
plot(chm)


###################################################################################################
# function for smoothing LiDAR-derived Canopy Height Model (CHM) 

#=======================================================================#
# Importing the LiDAR-derived CHM file
data(chm) # or set a CHM. e.g. chm<-raster("CHM_stand.asc")
#=======================================================================#
# Example 01: Smoothing the CHM using a Gaussian filter
#=======================================================================#
# Set the ws:
ws<-3 # dimension 3x3
# Set the filter type
filter<-"Gaussian"

# Set the sigma value
sigma<-0.6
# Smoothing CHM
sCHM<-CHMsmoothing(chm, filter, ws, sigma)
#=======================================================================#
# Example 02: Smoothing the CHM using a mean filter
#=======================================================================#
# Set the ws:
ws<-5 # dimension 5x5
# Set the filter type
filter<-"mean"
# Smoothing and plotting LiDAR-derived CHM
sCHM<-CHMsmoothing(chm, filter, ws)
##################################################################################################
#function for calculating 2D Convex hull of individual tree LiDAR-derived point cloud

# Importing LAS file:
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Reading LAS file
LAS<-readLAS(LASfile,short=TRUE)

# Height subsetting the data
xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)

# Getting LiDAR clusters
set.seed(1)
clLAS<-kmeans(xyz, 32)

# Set the points id
id<-as.factor(clLAS$cluster)

# Set the xyid input
xyid<-cbind(xyz[,1:2],id)

# Compute the LiDAR convex hull of the clusters
chullTrees<-chullLiDAR2D(xyid)

# Plotting the LiDAR convex hull
library(sp)
plot(SpatialPoints(xyid[,1:2]),cex=0.5,col=xyid[,3])
plot(chullTrees$chullPolygon,add=TRUE, border='green')

# Get the ground-projected area of LiDAR convex hull
chullList<-chullTrees$chullArea
summary(chullList) # summary
###################################################################################################
# function for calculating 3D convex hull of the individual tree LiDAR-derived point cloud

# Importing LAS file:
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Reading LAS file
LAS<-readLAS(LASfile,short=TRUE)

# Setring the xyz coordinates and subsetting the data
xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)

# Finding clusters
clLAS<-kmeans(xyz, 32)

# Set the id vector
id<-as.factor(clLAS$cluster)
#=================================================#
# Example 01
#=================================================#
# Set the alpha
alpha<-0.6

# Set the plotCAS parameter
plotit=TRUE

# Set the convex hull color
col="forestgreen"

# Combining xyz and id
xyzid<-cbind(xyz,id)

# Get the volume and surface area
library(rgl)
open3d()

volumeList<-chullLiDAR3D(xyzid=xyzid, plotit=plotit, col=col,alpha=alpha)
summary(volumeList) # summary

plot3d(xyzid[,1:3], add=TRUE) # add the 3D point cloud

axes3d(c("x+", "y-", "z-")) # axes

grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid

title3d(xlab = "UTM Easthing", ylab = "UTM Northing",zlab = "Height", col="red")

aspect3d(1,1,0.7) # scale
#=================================================#
# Example 02
#=================================================#
# Set the alpha
alpha<-0.85

# Set the plotCAS parameter
plotit=TRUE

# Set the convex hull color
col=levels(factor(id))

# Combining xyz and id
xyzid<-cbind(xyz,id)

# Get the volume and surface area
open3d()
volumeList<-chullLiDAR3D(xyzid=xyzid, plotit=plotit, col=col,alpha=alpha)
summary(volumeList)

# Add other plot parameters
plot3d(xyzid[,1:3], col=xyzid[,4], add=TRUE) # add the 3D point cloud
axes3d(c("x+", "y-", "z-")) # axes

grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM Easthing", ylab = "UTM Northing",zlab = "Height", col="red")

aspect3d(1,1,0.7) # scale
###################################################################################################

# function for calculating a set of LiDAR-derived individual tree crown metrics
# a cmplete list of metrics can be found in the package tutorial

#=======================================================================#
# Individual tree detection using K-means cluster
#=======================================================================#
# Importing LAS file:
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Reading LAS file
LAS<-readLAS(LASfile,short=TRUE)

# Setting the xyz coordinates and subsetting the data
xyzi<-subset(LAS[,1:4],LAS[,3] >= 1.37)

# Finding clusters (trees)
clLAS<-kmeans(xyzi[,1:2], 32)

# Set the tree id vector
Id<-as.factor(clLAS$cluster)

# Combining xyzi and tree id
xyziId<-cbind(xyzi,Id)
#=======================================================================#
# Computing individual tree LiDAR metrics
#=======================================================================#
TreesMetrics<-CrownMetrics(xyziId)
head(TreesMetrics)

###################################################################################################
# a function for Individual tree detection whitin the LiDAR-derived CHM
# background of the algorithm: The algorithm implemented in this function is local maximum 
#with a fixed window size

# Importing the LiDAR-derived CHM raster file
data(chm) # or set a CHM. e.g. chm<-raster("CHM_stand.asc")
# Smoothing CHM
schm<-CHMsmoothing(chm, "mean", 5)

# Setting the fws:
fws<-5 # dimention 5x5

# Setting the specified height above ground for detectionbreak
minht<-8.0

# Getting the individual tree detection list
treeList<-FindTreesCHM(schm, fws, minht)
summary(treeList)

# Plotting the individual tree location on the CHM
library(raster)
plot(chm, main="LiDAR-derived CHM")

library(sp)
XY<-SpatialPoints(treeList[,1:2]) # Spatial points
plot(XY, add=TRUE, col="red") # plotthing tree location

###################################################################################################
#function for Individual trees crown deliniation from LiDAR-derived CHM

# Import the LiDAR-derived CHM file
data(chm) # or set a CHM. e.g. chm<-raster("CHM_stand.asc")

# Set the loc parameter
sCHM<-CHMsmoothing(chm, filter="mean", ws=5) # smoothing CHM
loc<-FindTreesCHM(sCHM, fws=5, minht=8) # or import a tree list

# Set the maxcrown parameter
maxcrown=10.0

# Set the exclusion parameter
exclusion=0.3 # 30

# Compute individual tree detection canopy area
canopy<-ForestCAS(chm, loc, maxcrown, exclusion)
#==================================================================================#
# Retrieving the boundary for individual tree detection and canopy area calculation
#==================================================================================#
boundaryTrees<-canopy[[1]]

# Plotting the individual tree canopy boundary over the CHM
plot(chm, main="LiDAR-derived CHM")
plot(boundaryTrees, add=T, border='red', bg='transparent') # adding tree canopy boundary
#============================================================================#
# Retrieving the list of individual trees detected for canopy area calculation
#============================================================================#
canopyList<-canopy[[2]] # list of ground-projected areas of individual tree canopies
summary(canopyList) # summary

# Spatial location of the trees
library(sp)
XY<-SpatialPoints(canopyList[,1:2]) # Spatial points
plot(XY, col="black", add=T, pch="*") # adding tree location to the plot

###################################################################################################

#function for Computing LiDAR metrics that describe statistically the Lidar dataset
# most functions here correspond to those implemented by McGaughey, R. in FUSION/LDV software

#=======================================================================#
# Example 01: Computing LiDAR metrics for a single LAS file
#=======================================================================#
# Import the LAS data file
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Set the minht and above parameters
minht<-1.37 # meters or feet
above<-2.00 # meters or feet

# LiDAR metrics computation
LiDARmetrics<-LASmetrics(LASfile, minht, above)

#==========================================================================#
# Example 02: Computing Lidar metrics for multiple LAS files within a folder
#==========================================================================#
# Set folder where LAS source files reside
folder=dirname(LASfile)

# Get list of LAS files residing in the folder
LASlist <- list.files(folder, pattern="*.las", full.names=TRUE)

# Set the "minht" and "above" parameters
minht<-1.37 # meters or feet
above<-2.00 # meters or feet

# Create an empty dataframe in whic to store the LiDAR metrics
getMetrics<-data.frame()

# Set a loop to compute the LiDAR metrics
for ( i in LASlist) {
  getMetrics<-rbind(getMetrics, LASmetrics(i, minht, above))}

# Table of the Lidar metrics
LiDARmetrics<-cbind(Files=c(basename(LASlist)), getMetrics)
head(LiDARmetrics)

####################################################################################################
# function for 3D forest stand visualization
# Note that the function Draws a 3D scatterplot for individual trees detected from Lidar data.

#=======================================================================#
# EXAMPLE 01: Plotting single trees
#=======================================================================#
# cone crown shape
library(rgl)
open3d()
LiDARForestStand(crownshape = "cone", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)
# ellipsoid crown shape
open3d()
LiDARForestStand(crownshape = "ellipsoid", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)
# halfellipsoid crown shape
open3d()
LiDARForestStand(crownshape = "halfellipsoid", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)
# paraboloid crown shape
open3d()
LiDARForestStand(crownshape = "paraboloid", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)
# cylinder crown shape
open3d()
LiDARForestStand(crownshape = "cylinder", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)
# Set the shape=FALSE
open3d()
LiDARForestStand(crownshape = "paraboloid", CL = 10, CW =7,
                 HCB = 5, X =0, Y = 0, dbh = 0.4, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=FALSE)
#=======================================================================#
#EXAMPLE 02: Plotting a forest plantation stand in virtual 3-D space
#=======================================================================#
# Set the dimensions of the displayed forest stand
xlength<-30 # x length
ylength<-20 # y length
# Set the space between trees
sx<-3 # x space length
sy<-2 # y space length
# Tree location grid
XYgrid <- expand.grid(x = seq(1,xlength,sx), y = seq(1,ylength,sy))

# Get the number of trees
Ntrees<-nrow(XYgrid)
# Plot a virtual Eucalyptus forest plantation stand using the halfellipsoid tree crown shape

# Set stand trees parameters
meanHCB<-5 # mean of the height at canopy base
sdHCB<-0.1 # standard deviation of the height at canopy base
HCB<-rnorm(Ntrees, mean=meanHCB, sd=sdHCB) # height at canopy base
CL<-HCB # tree crown height
CW<-HCB*0.6 # tree crown diameter
open3d() # open a rgl window
# Plotting the stand
for( i in 1:Ntrees){
  LiDARForestStand(crownshape = "halfellipsoid", CL = CL[i], CW = CW[i],
                   HCB = HCB[i], X = XYgrid[i,1], Y = XYgrid[i,2], dbh = 0.4,
                   crowncolor = "forestgreen", stemcolor = "chocolate4",
                   resolution="high", mesh=TRUE)
}
# Add other plot parameters
axes3d(c("x-", "x-", "y-", "z"), col="gray") # axes
title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # set a terrain plane

# Plotting a virtual single-species forest plantation stand using "cone" tree crown shape
# Set parameters f trees growing within the virtual stand

meanHCB<-3 # mean of the height at canopy base
sdHCB<-0.1 # standard deviation of the height at canopy base
HCB<-rnorm(Ntrees, mean=meanHCB, sd=sdHCB) # height at canopy base
CL<-HCB*2.0 # tree crown height
CW<-HCB*1.3 # tree crown diameter
open3d() # open a rgl window

# Plot stand
for( i in 1:Ntrees){
  LiDARForestStand(crownshape = "cone", CL = CL[i], CW = CW[i],
                   HCB = HCB[i], X = XYgrid[i,1], Y = XYgrid[i,2], dbh = 0.4,
                   crowncolor = "forestgreen", stemcolor = "chocolate4",
                   resolution="high", mesh=TRUE)
}
# Add other plot parameters
axes3d(c("x-", "x-", "y-", "z"), col="gray") # axes
title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # set a terrain plane                 
 
#=======================================================================#
# EXAMPLE 03: Plotting a virtual mixed forest stand
#=======================================================================#
# 01. Plot different trees species in the stand with different crown shapes
# Set the number of trees 
Ntrees<-80

# Set the trees locations
xcoord<-sample(1:100, Ntrees) # x coord
ycoord<-sample(1:100, Ntrees) # y coord

# Set a location grid of trees
XYgrid<-cbind(xcoord,ycoord)

# Plot the location of the trees
plot(XYgrid, main="Tree location")
meanHCB<-7 # mean of the height at canopy base
sdHCB<-3 # standard deviation of height at canopy base
HCB<-rnorm(Ntrees, mean=meanHCB, sd=sdHCB) # height at canopy base
crownshape<-sample(c("cone", "ellipsoid","halfellipsoid",
                     "paraboloid"), Ntrees, replace=TRUE) # tree crown shape
CL<-HCB*1.3 # tree crown height
CW<-HCB # tree crown diameter
open3d() # open a rgl window

# Plot stand
for( i in 1:Ntrees){
  LiDARForestStand(crownshape = crownshape[i], CL = CL[i], CW = CW[i],
                   HCB = HCB[i], X = as.numeric(XYgrid[i,1]), Y = as.numeric(XYgrid[i,2]),
                   dbh = 0.4, crowncolor = "forestgreen", stemcolor = "chocolate4",
                   resolution="high", mesh=TRUE)
}

# Add other plot parameters
axes3d(c("x-", "x-", "y-", "z"), col="gray") # axes
title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # set a terrain plane

# 02. Plot different tree height in the stand using different crown colors
# Set the number of trees
Ntrees<-80
# Set the tree locations
xcoord<-sample(1:100, Ntrees) # x coord
ycoord<-sample(1:100, Ntrees) # y coord
# Set a location grid of trees
XYgrid<-cbind(xcoord,ycoord)
# plot the location of the trees
plot(XYgrid, main="Tree location")

meanHCB<-7 # mean of the height at canopy base
sdHCB<-3 # standard deviation of the height at canopy base

HCB<-rnorm(Ntrees, mean=meanHCB, sd=sdHCB) # height at canopy base
crownshape<-sample(c("cone", "ellipsoid","halfellipsoid", "paraboloid"),
                   Ntrees, replace=TRUE) # tree crown shape

CL<-HCB*1.3 # tree crown height
CW<-HCB # tree crown diameter
# Plot tree height based on the HCB quantiles
HCBq<-quantile(HCB) # HCB quantiles
crowncolor<-NA*(1:Ntrees) # set an empty crowncolor vector

# classify trees by HCB quantile
for (i in 1:Ntrees){
  if (HCB[i] <= HCBq[2]) {crowncolor[i]<-"red"} # group 1
  if (HCB[i] > HCBq[2] & HCB[i] <= HCBq[3] ) {crowncolor[i]<-"blue"} # group 2
  if (HCB[i] > HCBq[3] & HCB[i] <= HCBq[4] ) {crowncolor[i]<-"yellow"} # group 3
  if (HCB[i] >= HCBq[4]) {crowncolor[i]<-"dark green"} # group 4
}

open3d() # open a rgl window

# Plot stand
for(i in 1:Ntrees){
  LiDARForestStand(crownshape = crownshape[i], CL = CL[i], CW = CW[i],
                   HCB = HCB[i], X = as.numeric(XYgrid[i,1]), Y = as.numeric(XYgrid[i,2]),
                   dbh = 0.4, crowncolor = crowncolor[i],stemcolor = "chocolate4",
                   resolution="high", mesh=TRUE)
}

# Add other plot parameters
axes3d(c("x-", "x-", "y-", "z"), col="gray") # axes
title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # set a terrain plane


###################################################################################################

###########################################END OF CODE#############################################
###################################################################################################
















