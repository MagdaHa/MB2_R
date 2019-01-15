##########################################################################
####examples of working with airborne point clouds inside lidR library####
##########################################################################

#Airborne LiDAR interface for data
#manipulation and visualization. Read/write 'las' and 'laz' files, computation
#of metrics in area based approach, point filtering, artificial point reduction,
#classification from geographic data, normalization, individual tree object segmentation
#and other processing steps
#-----------------------------------------------------------------------------------------------

#load the package
library (lidR)
library(sp)
library(raster)
#----------------------------------------------------------------------------------------------

# examples for LidR package

#----------------------------------------------------------------------------------------------
# area of a 3d point cloud object (computed in the same units as the coordinate reference system)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)
area(las)

# plot las data point cloud

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
plot(lidar)
# Outliers of intensity breaks the color range. Use the trim parameter.

plot(lidar, color = "Intensity", colorPalette = heat.colors(50))
plot(lidar, color = "Intensity", colorPalette = heat.colors(50), trim = 0.99)

#-----------------------------------------------------------------------------------------------
# DTM from a set of ground points using different interpolation methods

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
lidar = readLAS(LASfile)
plot(lidar)
dtm1 = grid_terrain(lidar, algorithm = knnidw(k = 6))   #different interpolation methods for dtm1, dtm2, dtm3
dtm2 = grid_terrain(lidar, algorithm = tin())
dtm3 = grid_terrain(lidar, algorithm = kriging(k = 10))

#plot 2D
par(mfrow=c(1,3))
plot(dtm1)
plot(dtm2)
plot(dtm3)
#pot3D
plot_dtm3d(dtm1)      #3D plot
plot_dtm3d(dtm2)
plot_dtm3d(dtm3)

#------------------------------------------------------------------------------------------------
# CANOPY SURFACE MODEL (CSM) (i.e. canopy height model, CHM) using a point cloud
# For each pixel the function returns
# the highest point found (point-to-raster)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
# Local maximum algorithm with a resolution of 2 meters
chm1 = grid_canopy(lidar, 2, algorithm=pitfree())  #2nd argument is resolution, res 2m
plot(chm1)

# Local maximum algorithm with a resolution of 1 meter replacing each
# point by a 20 cm radius circle of 8 points
chm2 = grid_canopy(lidar, 1, algorithm=pitfree())  #res 1m
plot(chm2)

par(mfrow=c(1,2))
plot(chm1, main="2m spatial resolution")
plot(chm2, main="1m spatial resolution")

# Local maximum algorithm with a resolution of 1 meter replacing each
# point by a 10 cm radius circle of 8 points and interpolating the empty
# pixels using the 3-nearest neighbours and an inverse-distance weighting.
chm = grid_canopy (lidar, 1, subcircle = 0.1, na.fill = "knnidw", k = 3, p = 2)
plot(chm)
## Not run:
chm = grid_canopy(lidar, 1, na.fill = "knnidw", k = 3)
plot(chm)
chm = grid_canopy(lidar, 1, subcircle = 0.1, na.fill = "delaunay")
plot(chm)
## End(Not run)

#---------------------------------------------------------------------------------------------
#  pulse or point DENSITY in a lidar point cloud

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
d5 = grid_density(lidar, 5)
plot(d5)

# yet with another grid size
d10 = grid_density(lidar, 10)
plot(d10)

#----------------------------------------------------------------------------------------------
# VVOXELIZE the cloud of points
# and compute a series of descriptive statistics for each voxel

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
summary(lidar)

# Cloud of points is voxelized with a 3-meter resolution and in each voxel
# the number of points is computed.
grid_metrics3d(lidar, length(Z), 3)

# Cloud of points is voxelized with a 3-meter resolution and in each voxel
# the mean scan angle of points is computed.
grid_metrics3d(lidar, mean(ScanAngle), 3)

# Define your own metric function
myMetrics = function(i, angle)
{
  ret = list(
    npoints = length(i),
    angle = mean(angle),
    imean = mean(i)
  )
  return(ret)
}

# plot them
voxels = grid_metrics3d(lidar, myMetrics(Intensity, ScanAngle), 3)
plot(voxels, color = "angle")
plot(voxels, color = "imean")

#-----------------------------------------------------------------------------------------
# CLIP a given point cloud
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")

# Load the file and clip the region of interest
las = readLAS(LASfile)
subset1 = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)

# Do not load the file, extract only the region of interest
ctg = catalog(LASfile)
subset2 = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)

# Extract a polygon from a shapefile
shapefile_dir <- system.file("extdata", package = "lidR")
lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
lake = lakes@polygons[[1]]@Polygons[[1]]
subset3 = lasclip(ctg, lake)

# Extract a polygon, write it in a file, do not load anything in R
file = paste0(tempfile(), ".las")
lasclip(ctg, lake, ofile = file)

## plot
plot(subset1)
plot(subset2)
plot(subset3)

#----------------------------------------------------------------------------------------------
# CLASSIFY the point clouds based on classes defined using an external data source
# lasclassify DOES NOT EXIST ANYMORE

#LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#shapefile_dir <- system.file("extdata", package = "lidR")
#lidar = readLAS(LASfile)
#lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")

# The field "inlake" does not exist in the shapefile. Points are classified as TRUE if in a polygon
#lasclassify(lidar, lakes, "inlakes") # New column 'inlakes' is added.
#forest = lasfilter(lidar, inlakes == FALSE)
#plot(lidar)
#plot(forest)

# The field "LAKENAME_1" exists in the shapefile.
# Points are classified with the values of the polygons
#lasclassify(lidar, lakes, "LAKENAME_1") # New column 'LAKENAME_1' is added.

#-----------------------------------------------------------------------------------------------
# THIN a point cloud based on defined point density of pattern
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile, select = "xyz")

# By default the method is homogenize = TRUE
thinned = lasfilterdecimate(lidar, algorithm = random(2))

#plot 2D lidar and thinned (density)
par(mfrow=c(1,2))
plot(grid_density(lidar))
plot(grid_density(thinned))

#plot 3D lidar and thinned data
plot(lidar)
plot(thinned)

# Method homogenize = FALSE enables a global pulse density to be reached
#thinned = lasfilterdecimate(lidar, 1, homogenize = FALSE)
#summary(thinned)
#d = grid_density(thinned)
#plot(d)

#-----------------------------------------------------------------------------------------------
# FILTER the point clouds based on specific returns: FIRST/LAST returns

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
firstReturns = lasfilterfirst(lidar)
groundReturns = lasfilterground(lidar)

#plot 3D
plot(firstReturns)
plot(groundReturns)

#-------------------------------------------------------------------------------------------------
# CLASSIFY the point cloud into ground and non-ground points

#LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#las = readLAS(LASfile, select = "xyzRN")
#ws = seq(3,12, 3)
#th = seq(0.1, 1.5, length.out = length(ws))
#lasground(las, algorithm=pmf(ws, th))

#plot(las, color = "Classification")

#--------------------------------------------------------------------------------------------
# STATISTICAL METRICS for point clouds
# the user should write own functions for computing the metrics

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)
lasmetrics(lidar, max(Z))
lasmetrics(lidar, mean(ScanAngle))

# Define your own new metrics
myMetrics = function(z, i)
{
  metrics = list(
    zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
    zimean = mean(z*i), # Mean products of z by intensity
    zsqmean = sqrt(mean(z^2)) # Quadratic mean

  )
  return(metrics)
}
metrics = lasmetrics(lidar, myMetrics(Z, Intensity))

# Predefined metrics
lasmetrics(lidar, .stdmetrics)

#---------------------------------------------------------------------------------------------
# DESCRIPTIVE METRICS on individual tree level

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, filter = "-drop_z_below 0")

# segment trees (see lastrees)
lastrees(las, algorithm = li2012(R=3, speed_up = 5))

# Max height for each tree
tree_metrics(las, mean(Z))

# Define your own new metrics
myMetrics = function(z, i)
  
{
  metrics = list(
    imean = mean(i),
    imax = max(i),
    npoint = length(z)
  )
  return(metrics)
}
metrics = tree_metrics(las, myMetrics(Z, Intensity))

# predefined metrics (see ?stdmetrics)
metrics = tree_metrics(las, .stdtreemetrics)

#--------------------------------------------------------------------------------------------
# PLOT the set of 3d point cloud metrics based on a GRIDDED SURFACE
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

# Canopy surface model with 4 m^2 cells
plot(grid_canopy(las, res=2, algorithm = pitfree()))

# Mean height with 400 m^2 cells
plot(grid_metrics(las, mean(Z)))

# With multiple metrics
metrics = grid_metrics(las, .stdmetrics_z)

#plot
plot(metrics)
plot(metrics, "zmean")
plot(metrics, "zmax")

#--------------------------------------------------------------------------------------------
# NORMALIZE the points bz subtracting dtm from the point cloud
#dtm (i.e. normalization)can also be computed on the fly, on a point-to-point basis

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
plot(las)

# --- First option: compute a raster DTM with grid_terrain ---
# (or read it from a file)
dtm = grid_terrain(las, method = "kriging", k = 10L)
lasnormalize(las, dtm)
plot(dtm)
plot(las)

# --- Second option: interpolate each point (no discretization) ---
las = readLAS(LASfile)
lasnormalize(las, method = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
plot(las)

#TEST: see if the Z values are normalized!
las@data$Z

#------------------------------------------------------------------------------------------------
# function to interactively select a region of interest from a point cloud data 

#LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#lidar = readLAS(LASfile)
#subset = lasroi(lidar)

#------------------------------------------------------------------------------------------------
#SMOOTHING operations on a point cloud: 
# average within a window and Gaussian smooth within a window

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, select = "xyz")
las = lasfiltersurfacepoints(las, 1)
plot(las)

lassmooth(las, 5, "gaussian", "circle", sigma = 2)
plot(las)

#lasunsmooth(las)
#plot(las)

#-------------------------------------------------------------------------------------------------
## INDIVIDUAL TREES segmentation algorithms on point cloud data
## the algorithms include "dalponte2016", "watershed","li2012"(deprecated), "li2012-2" or "silva2016"

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
col = pastel.colors(200)

# algorithm Li 2012
lastrees <- lastrees(las, algorithm = li2012(R = 3, speed_up = 5))
plot(lastrees, color = "treeID", colorPalette = col)

#------------------------------------------------------------------------------------------------
# TREE TOP DETECTION based on a local maxima filter algorithms
# there are two types of filters:
# 1-for gridded objects, works on images with a matrix-based algorithm 
# 2- for point clouds, works at the point cloud level without any rasterization.

#LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")

# point-cloud-based method
#ttops = tree_detection(las, 5)
#plot(las)
#with(ttops, rgl::points3d(X, Y, Z, col = "red", size = 5, add = TRUE))

# raster-based method
#chm = grid_canopy(las, 1, subcircle = 0.15)
#chm = as.raster(chm)
#kernel = matrix(1,3,3)
#chm = raster::focal(chm, w = kernel, fun = median, na.rm = TRUE)
#ttops = tree_detection(chm, 5)
#raster::plot(chm, col = height.colors(30))
#raster::plot(ttops, add = TRUE, col = "black", legend = FALSE)

#--------------------------------------------------------------------------------------
# HULLSA of each segmented tree.
# The hull can be 1)convex, 2)concave or a 3)bounding box 

#LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")

#las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#lastrees_li2(las, speed_up = 7)
#convex_hulls = tree_hulls(las)
#sp::plot(convex_hulls)
#bbox_hulls = tree_hulls(las, "bbox")
#sp::plot(bbox_hulls)

#concave_hulls = tree_hulls(las, "concave")
#sp::plot(concave_hulls)

#---------------------------------------------------------------------------------------