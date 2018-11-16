#############################
####importing raster data####
#############################

##creating own raster
#creating an empty raster dataset r1
library(raster)
r1 <- raster(nrows=10, ncols=10)
#filling empty raster with 100 random values
r1[] <- rnorm(100) # [] uses r1 and replaces the empty raster with random data
r1
plot(r1)
#or
r1[] <- df$measure1[1:100]
plot(r1)

#create a second raster dataset r2
r2 <- raster(nrows=10, ncols=10)
#filling empty raster with 100 random values
r2[] <- rnorm(100) # [] uses r1 and replaces the empty raster with random data
r1
plot(r2)
#or
r2[] <- df$measure2[1:100]
plot(r2)

#-----------------------------------------------------------------------------------
##loading pregenerated raster, like satellite imagery
library(raster)
dgm <- raster("C:/Bachelorarbeit/Aster GDEM/Arbeitsgebiet_clip.tif")
plot(dgm)
