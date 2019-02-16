########################################################################
# Exercise
# 
########################################################################

# Setting the working directory and paths
setwd("C:/Users/cord/ownCloud/Meine_Daten/Lehre/2019/WUE_EAGLE/Praktischer_Teil/SDM")

####################### Load additional functions ########################
source("pairs_correlation_diagram.r")    # from http://statisticsr.blogspot.com/2009/12/r-pairs-plot.html 

####################### Load packages ########################

library(raster)  # for raster data
library(rgdal)
library(dismo) # random points, model evaluation
library(randomForest) # RandomForest

#####################################################################
############################ EXERCISE PART A ########################
#####################################################################

# 1) Import the shapefile "Bursera_simaruba.shp" (same projection as the Pinus shape file)
# 2) Plot the species data, check for duplicates, find out how many records we have.
# 3) Import remotely sensed predictors from the folder "modis", set the NA value to -3000, make a layer stack and plot the data.


####################### Load species and GIS data ########################

# Species occurrence records
Bursera.shp <- readOGR(getwd(), "Bursera_simaruba")
projection(Bursera.shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
plot(Bursera.shp, col="red", pch=16)
head(Bursera.shp@data)
# Which species are included in this file?
unique(Bursera.shp$SPECIES)

# Check for duplicates
dups <- duplicated(Bursera.shp@data)
sum(dups) # no duplicates

# How many records do we have?
nrow(Bursera.shp@data) # 3961

####################### Load remote sensing predictors ########################

# Create a filelist with all files in a directory
filelist.rs <- list.files(path=paste(getwd(),"/modis/", sep=""), pattern = "*.tif", full.names=TRUE)
print(filelist.rs)

# Make a RasterSteck
modis <- stack(filelist.rs)
NAvalue(modis) <- -3000     # Set NA value to -3000
dim(modis) # x,y, and z dimensions

# Make plots
plot(modis)


#####################################################################
############################ EXERCISE PART B ########################
#####################################################################

# 4) Generate 10,000 random absence points in Mexico.
# 5) Combine the presence and absence points in 1 dataframe (called "B.sim". (set the column SPECIES to "0" for absence and to "1" for presence).
# 6) Extract the values at the presence/absence localities in B. sim from the MODIS remote sensing layers, remove rows with NA values. 

# Note: 
# Since B.sim is a (non-spatial) data frame, use the following lines:
# xy <- cbind(B.sim$LONGITUDE, B.sim$LATITUDE)
# B.sim.sp <- SpatialPoints(xy)
# env <- extract(modis, B.sim.sp)


# Generate random absence data
mask <- modis[[1]]
set.seed(345)
bg <- randomPoints(mask, 1000)
par(mfrow=c(1,1))
plot(bg)
head(bg)

# Add column for species
bg <- cbind(bg,0)
colnames(bg) <- c("LONGITUDE", "LATITUDE","SPECIES")
head(bg)

# Reorder columns 
bg <- bg[,c("SPECIES","LONGITUDE", "LATITUDE")]
head(bg)

# Set SPECIES to 1 in the presence data
Bursera.shp$SPECIES <- 1
head(Bursera.shp)

# Combine presence and absence data
B.sim <- rbind(Bursera.shp@data,bg)
View(B.sim) # just to check

# Extract values at species localities from environmental layers
xy <- cbind(B.sim$LONGITUDE, B.sim$LATITUDE)
B.sim.sp <- SpatialPoints(xy)
env <- extract(modis, B.sim.sp)
head(env)

# Combine
B.sim <- cbind(B.sim,env)

View(B.sim)

# Check for NA values
sum(is.na(B.sim))
B.sim <- na.omit(B.sim)
sum(is.na(B.sim)) # no NAs left


#####################################################################
############################ EXERCISE PART C ########################
#####################################################################

# 7) Using B.sim, create random subsets of the species presence data for training (75% of data) and testing (25% of data).
# 8) Implement a Bioclim species distribution model for Bursera simaruba (since Bioclim is a presence-only method, you can use all absence data for testing) using all MODIS data (including model prediction and a simple map of the result).
# 9) Evaluate the model based on AUC, using 5-fold data partitioning.


# Random subsets
B.sim.pres <- B.sim[B.sim$SPECIES ==1,]
samp<- sample(nrow(B.sim.pres), round(0.75 * nrow(B.sim.pres)))
samp
traindata.pres <- B.sim.pres[samp,]
nrow(traindata.pres)
testdata.pres <- B.sim.pres[-samp,]
nrow(testdata.pres)


# Bioclim model
selected <- colnames(B.sim)[4:9]
selected
bc <- bioclim(traindata.pres[,selected]) 
class(bc)
bc

# Obtain response curves
response(bc)

# Make prediction
p <- predict(modis, bc)
plot(p)
points(Bursera.shp, col="blue")

# Model evaluation based on AUC
k <- 5
group <- kfold(B.sim.pres, k)
group
unique(group)

e <- list()
for (i in 1:k) {
  train <- B.sim.pres[group != i,]
  test <- B.sim.pres[group == i,]
  bc <- bioclim(train[,selected])
  e[[i]] <- evaluate(p=test, a=B.sim[B.sim$SPECIES == 0,], bc)  # using the same absence data as above
}

# AUC
auc <- sapply(e, function(x){slot(x, 'auc')} )
auc
mean(auc)

