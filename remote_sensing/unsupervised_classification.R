#####################################
#### unsupervised classification ####
#####################################

library(RStoolbox)
#library(raster)

#loading sample data
data(lsat)
#allbands <- brick(lsat)   #extracts bands out of the lsat layer -> raster brick


#classify with 6 classes
uc <- unsuperClass(lsat, nClasses = 6)
plot(uc$map)
