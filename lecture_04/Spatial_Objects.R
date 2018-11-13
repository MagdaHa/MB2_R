#############################
#######SPATIAL OBJECTS#######
########day04 reading########
#############################
library(sp)

#-----------------CLASS: Spatial Objects------------------#
getClass("Spatial")               #slots: bbox=matrix; prof4string=CRS           bbox = predefined extent

#example
bbox1 <- matrix(c(0,0,1,1), ncol = 2, dimnames=list(NULL, c("min", "max"))) #matrix with bbox attribute
p4s1 <- CRS(projargs = as.character(NA))  #CRS object with reference system; NA=missing reference

SpatialObject <- Spatial(bbox = matrix1, proj4string = p4s1)  # creating the spatial object
SpatialObject

summary(SpatialObject)                     #printing matrix data with min and max xy coordinates
proj4string(SpatialObject)                 #printing predefined reference system

proj4string(SpatialObject) <- CRS(as.character(NA))  #changing the reference system


#-----------------Subclass: SpatialPoints-----------------#
getClass("SpatialPoints")         #slots: coords=matrix; bbox=matrix; proj4string=CRS

#example
df <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
names(df)                                 #printing column names
coords2 <- cbind(df$x, df$y)              #defining columns with coordinates
p4s2 <- CRS("+proj=longlat +ellps=WGS84") #spatial reference

SpatialPoint <- SpatialPoints(coords = coords2, proj4string = p4s2) #creating the spatial point object

summary(SpatialPoint)                     #printing matrix data with min and max xy coordinates
coordinates(SpatialPoint)                 #printing all defined coordinates
proj4string(SpatialPoint)                 #printing predefined reference system


#example SpatialPointsDataFrame (matching points to a specific dataframe)
spdf1 <- SpatialPointsDataFrame(coords2, df, proj4string = p4s2, match.ID = TRUE)
summary(spdf1)


#---------------Subclass: SpatialLines-------------------#
getClass("SpatialLines")          #slots: lines=list; bbox=matrix; Proj4string=CRS

#example
#install.packages("maps")
#install.packages("maptools")
library(maps)
library(maptools)


linesgermany <- map("world", "germany", plot = TRUE)  #defining lines via maps package
p4s3 <- CRS("+proj=longlat +ellps=WGS84") #defining coordinate system
SpatialLines <- map2SpatialLines (linesgermany, proj4string = p4s3)
str(SpatialLines, max.level = 2)

summary(SpatialLines)                     #printing matrix data with min and max xy coordinates
coordinates(SpatialLines)                 #printing all defined coordinates
proj4string(SpatialLines)                 #printing predefined reference system

Lines_len <- sapply(slot(SpatialLines, "lines"), function (x) length(slot(x, "Lines")))
table(Lines_len)                          #printing the number of line objects


#--------------Subclass: SpatialPolygon------------------#
getClass("Polygon")               #LinearRing Objects; extends the Line class --> adding slots (labelpoint)
                                  #slots: labpt=numeric; area=numeric; hole=logical; ringDIR=integer; coords=matrix
getClass("Polygons")              #MultiPolygon objects; list of valid Polygon objects
                                  #slots: Polygons=list; PlorOrder=integer; labpt=numeric; ID=character; area=numeric
getClass("SpatialPolygons")       #set of Polygon objects + slots of Spatial objects
                                  #slots: polygons=list; plotOrder=integer; bbox=matrix; proj4string=CRS

#example SpatialPolygonsDataFrame
library(maps)
state.map <- map("state", plot = TRUE, fill = FALSE)  #printing polygon outlines of US states
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])

library(maptools)

#####was falsch?######
state.sp <- map2SpatialPolygons(state.map, IDs =IDs, proj4string = CRS("+proj=longlat +ellps=WGS84"))


#-------------------------------------------------------#
