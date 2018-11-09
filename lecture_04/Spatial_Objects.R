#############################
#######SPATIAL OBJECTS#######
########day04 reading########
#############################
library(sp)

#-----------------CLASS: Spatial Objects------------------#
getClass("Spatial")               #bbox=matrix; prof4string=CRS

#example
bbox1 <- matrix(c(0,0,1,1), ncol = 2, dimnames=list(NULL, c("min", "max")))
p4s1 <- CRS(projargs = as.character(NA))

SpatialObject <- Spatial(bbox = matrix1, proj4string = p4s1)
SpatialObject


#-----------------Subclass: SpatialPoints-----------------#
getClass("SpatialPoints")         #coords=matrix; bbox=matrix; proj4string=CRS

#example
df <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
names(df)                                 #printing column names
coords2 <- cbind(df$x, df$y)              #defining columns with coordinates
bbox2 <- matrix(df$SRTM, df$LUCAS_LC)
p4s2 <- CRS("+proj=longlat +ellps=WGS84")

###was falsch?######
SpatialPoint <- SpatialPoints(coords = coords2, bbox = bbox2, proj4string = p4s2)
summary(SpatialPoint)

ndvi <- which(df$L7.ndvi>=0.8)
ndvi
coordinates(SpatialPoint) [ndvi, ]        #printing matrix data only where ndvi>=0.8
summary(SpatialPoint[ndvi, ])


#---------------Subclass: SpatialLines-------------------#
getClass("SpatialLines")          #lines=list; bbox=matrix; Proj4string=CRS

#example
#install.packages("maps")
#install.packages("maptools")
library(maps)
library(maptools)


linesgermany <- map("world", "germany", plot = TRUE)  #defining lines via maps package
p4s3 <- CRS("+proj=longlat +ellps=WGS84") #defining coordinate system
SpatialLines <- map2SpatialLines (linesgermany, proj4string = p4s3)
str(SpatialLines, max.level = 2)
summary(SpatialLines)                     #printing matrix data with xy coordinates

Lines_len <- sapply(slot(SpatialLines, "lines"), function (x) length(slot(x, "Lines")))
table(Lines_len)                        #printing how many line objects


#--------------Subclass: SpatialPolygon------------------#
getClass("Polygon")               #LinearRing Objects; extends the Line class --> adding slots (labelpoint)
                                  #labpt=numeric; area=numeric; hole=logical; ringDIR=integer; coords=matrix
getClass("Polygons")              #MultiPolygon objects; list of valid Polygon objects
                                  #Polygons=list; PlorOrder=integer; labpt=numeric; ID=character; area=numeric
getClass("SpatialPolygons")       #set of Polygon objects + slots of Spatial objects
                                  #polygons=list; plotOrder=integer; bbox=matrix; proj4string=CRS

#example SpatialPolygonsDataFrame
library(maps)
state.map <- map("state", plot = TRUE, fill = FALSE)  #
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])

library(maptools)

#was falsch?######
state.sp <- map2SpatialPolygons(state.map, IDs =IDs, proj4string = CRS("+proj=longlat +ellps=WGS84"))


#------------
