########################################################################
# Simple Species Distribution Model for different Mexican tree species
# 
# Anna Cord
# Partly based on: "Species distribution modeling with R", by Robert J. Hijmans and Jane Elith (2017)
# 2019
########################################################################

# Setting the working directory and paths
setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\RE2_Vegetation and Biogeography_Werner\\Block_course\\R\\sdm")

####################### Load additional functions ########################
source("pairs_correlation_diagram.r")    # from http://statisticsr.blogspot.com/2009/12/r-pairs-plot.html 

####################### Load packages ########################

library(raster)  # for raster data
library(rgdal)
library(dismo) # random points, model evaluation
library(randomForest) # RandomForest

#####################################################################
################# Data import, cleaning and visualization ###########
#####################################################################


####################### Load species and GIS data ########################

# In most cases you will have a file with point locality data representing the known distribution of a species. In this case, it is a shapefile and we use the function readOGR() to important it. In other cases, these are often csv or txt files that can be imported using read.csv()or read.table().

# Species occurrence records
Pinus.shp <- readOGR(getwd(), "Pinus")
projection(Pinus.shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Pinus.shp does not have a prj file, so the projection needs to be set
plot(Pinus.shp, col="red", pch=16)
head(Pinus.shp@data) #@ accesses the attribute table
# Which species are included in this file?
unique(Pinus.shp$SPECIES)
# How many records per species do we have?
number.records <- summary(Pinus.shp$SPECIES)
number.records

# We will now import some additional GIS layers, for plotting and illustration purposes only
study.area.shp <- readOGR("./GIS", "study_area") # readOGR recognizes projections based on the prj file
protected.areas.shp <- readOGR("./GIS", "protected_areas_Mexico")

# Display data (all in one diagram with add=T)
plot(study.area.shp, col="grey90")            # study area
plot(protected.areas.shp, col="green", add=T)  # study area with boundaries of protected areas
points(Pinus.shp, col="blue") # "add=T" is implicit with the points function


####################### Load environmental predictors ########################

# In species distribution modeling, predictor variables are typically organized as raster (grid) type files. Each predictor should be a 'raster' representing a variable of interest. Variables can include climatic, soil, terrain, vegetation, land use, and other variables. These data are typically stored in files in some kind of GIS format. Almost all relevant formats can be used (including ESRI grid, GeoTiff, netCDF, IDRISI). Avoid ASCII files if you can, as they tend to considerably slow down processing speed. For any particular study the layers should all have the same spatial extent, resolution, origin, and projection. If necessary, use functions like crop, extend, aggregate, resample, and projectRaster from the 'raster' package to prepare your predictor variable data.

# Here, we will use (already downloaded) data representing 'bioclimatic variables' from the WorldClim database (http://www.worldclim.org, Hijmans et al., 2005)

# Create a filelist with all files in a directory
filelist.bioclim <- list.files(path=paste(getwd(),"/bioclim/", sep=""), pattern = "*.tif", full.names=TRUE)
print(filelist.bioclim)

# Make a RasterSteck
climate <- stack(filelist.bioclim)
NAvalue(climate) <- -3000     # Set NA value to -3000
climate   # check data range again
dim(climate) # x,y, and z dimensions

# Make plots
plot(climate) #plot all BIO data in one plot
plot(climate, "bio1") # plot BIO1 (Annual mean temperature)
plot(climate, "bio12") # plot BIO12 (Annual precipitation)
           

#################### Analyze environmental conditions at species localities ########################

# Extract values at species localities from environmental layers
Pinus.climate<- extract(climate,Pinus.shp)
head(Pinus.climate)
Pinus.climate <- cbind(Pinus.shp@data,Pinus.climate)#combining data with cbind
head(Pinus.climate)
dim(Pinus.climate)
write.csv(Pinus.climate, "Pinus_climate.csv")

########################### Data cleaning for species data #########################################

# Check localities of all species, e.g. none in the ocean?

# Remove duplicates
dups <- duplicated(Pinus.climate)
sum(dups) # How many duplicates?
which(dups==T) # You can check these lines in Excel if you want
Pinus.climate <- Pinus.climate[!dups, ] # keep only records which are not duplicates
dim(Pinus.climate)

# Check for NA values
sum(is.na(Pinus.climate))
Pinus.climate <- na.omit(Pinus.climate)
head(Pinus.climate)
sum(is.na(Pinus.climate)) # no NAs left

########################### Data visualization ######################

# Subsets for selected species
Pinus.maximinoi <- Pinus.climate[Pinus.climate$SPECIES=="Pinus_maximinoi",] 
Pinus.maximinoi$SPECIES <- 1
Pinus.californiarum <- Pinus.climate[Pinus.climate$SPECIES=="Pinus_californiarum",]
Pinus.californiarum$SPECIES <- 1
Pinus.teocote <- Pinus.climate[Pinus.climate=="Pinus_teocote",]
Pinus.teocote$SPECIES <- 1

# Boxplots
par(mfrow=c(1,2), mar=c(3,7,2,1))
boxplot(Pinus.maximinoi$bio1/10,Pinus.californiarum$bio1/10,Pinus.teocote$bio1/10, 
        names=c("P. maximinoi","P. californiarum","P. teocote"), 
        ylab="bio1 (Annual mean temperature)[?C]")
boxplot(Pinus.maximinoi$bio12,Pinus.californiarum$bio12,Pinus.teocote$bio12, 
        names=c("P. maximinoi","P. californiarum","P. teocote"), 
        ylab="bio12 (Annual precipitation)[mm]")

# Maps for the three species
par(mfrow=c(1,1))
plot(study.area.shp, col="grey90")            # study area
points(Pinus.shp[Pinus.shp$SPECIES=="Pinus_maximinoi",], col="blue") # "add=T" is implicit with the points function
points(Pinus.shp[Pinus.shp$SPECIES=="Pinus_californiarum",], col="red") 
points(Pinus.shp[Pinus.shp$SPECIES=="Pinus_teocote",], col="green") 
legend("bottomleft",col=c("blue","red","green"), c("P. maximinoi","P. californiarum","P. teocote"), pch=1, text.font=3)

############## Correlation analysis of environmental predictors ########################
cor(Pinus.maximinoi[,4:22]) # for all variables

pairs(Pinus.maximinoi[,4:10], upper.panel=panel.cor, diag.panel=panel.hist) # plot for only a few variables

# Remove highly correlatedvariables step by step
pairs(Pinus.maximinoi[,5:10], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(Pinus.maximinoi[,c(5,6,8,9,10)], upper.panel=panel.cor, diag.panel=panel.hist) 
pairs(Pinus.maximinoi[,c(5,6,8,9)], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(Pinus.maximinoi[,c(6,8,9)], upper.panel=panel.cor, diag.panel=panel.hist) 

# We here now select the following four not highly-correlated variables
selected <- c("bio15", "bio18", "bio19", "bio2")

pairs(Pinus.maximinoi[,selected], upper.panel=panel.cor, diag.panel=panel.hist) 

#####################################################################
######################### EXERCISE PART A ###########################
#####################################################################

# Goal: Repeat the previous steps focusing on a different species, Bursera simaruba.
# 1) Import the shapefile "Bursera_simaruba.shp" (same projection as the Pinus shape file)
# Species occurrence records
bursera_simaruba.shp <- readOGR(getwd(), "Bursera_simaruba")
projection(bursera_simaruba.shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Pinus.shp does not have a prj file, so the projection needs to be set
plot(bursera_simaruba.shp, col="red", pch=16)
head(bursera_simaruba.shp@data) #@ accesses the attribute table

# Which species are included in this file?
unique(bursera_simaruba.shp$SPECIES)

# How many records per species do we have?
number.records <- summary(bursera_simaruba.shp$SPECIES)
number.records

# We will now import some additional GIS layers, for plotting and illustration purposes only
study.area.shp <- readOGR("./GIS", "study_area") # readOGR recognizes projections based on the prj file
protected.areas.shp <- readOGR("./GIS", "protected_areas_Mexico")

#----------------------
# 2) Plot the species data, check for duplicates, find out how many records we have.
# Display data (all in one diagram with add=T)
plot(study.area.shp, col="grey90")            # study area
plot(protected.areas.shp, col="green", add=T)  # study area with boundaries of protected areas
points(bursera_simaruba.shp, col="blue") # "add=T" is implicit with the points function

# Extract values at species localities from environmental layers ?????????
BS.climate<- extract(modis,bursera_simaruba.shp)
head(BS.climate)
BS.climate <- cbind(bursera_simaruba.shp@data,BS.climate)#combining data with cbind
head(BS.climate)
dim(BS.climate)
write.csv(BS.climate, "Bursera_simaruba_climate.csv")

# Remove duplicates
dups <- duplicated(BS.climate)
sum(dups) # How many duplicates?
#which(dups==T) # You can check these lines in Excel if you want
#BS.climate <- BS.climate[!dups, ] # keep only records which are not duplicates
#dim(BS.climate)

# Check for NA values
sum(is.na(BS.climate))
BS.climate <- na.omit(BS.climate)
head(BS.climate)
sum(is.na(BS.climate)) # no NAs left

#----------------------
# 3) Import remotely sensed predictors from the folder "modis", set the NA value to -3000, make a layer stack and plot the data.
# Create a filelist with all files in a directory
filelist.modis <- list.files(path=paste(getwd(),"/modis/", sep=""), pattern = "*.tif", full.names=TRUE)
print(filelist.modis)

# Make a RasterSteck
modis <- stack(filelist.modis)
NAvalue(modis) <- -3000     # Set NA value to -3000
modis   # check data range again
dim(modis) # x,y, and z dimensions

# Make plots
plot(modis) #plot all MODIS data in one plot
plot(modis, "LST_mean") # plot LST_mean (mean surface temperature)


############################# END OF PART A #########################

# From now on we focus only on Pinus maximinoi!


#####################################################################
################ Generate species (pseudo)absence data ##############
#####################################################################

# Some of the early species distribution model algorithms, such as Bioclim and Domain only use 'presence' data in the modeling process. Other methods also use 'absence' data or 'background' data. If you have a large dataset with presence/absence from a well designed survey, you should use a method that can use these data (i.e. do not use a modeling method that only considers presence data). If you only have presence data, you can still use a method that needs absence data, by substituting absence data with background data. Background data are not attempting to guess at absence locations, but rather to characterize environments in the study region. In this sense, background is the same, irrespective of where the species has been found. 

# Approach 1: Generate random absence data
mask <- climate[[1]]
# Select 500 random points, use "set.seed" to assure that the examples will always have the same random sample
set.seed(1963)
bg <- randomPoints(mask, 500)
par(mfrow=c(1,1))
plot(bg)
head(bg)

# Approach 2: Target-group background approach (use absence data of other species to take into account the potential spatial sampling bias), we will use this datset in the following
abs <- Pinus.climate[Pinus.climate$SPECIES != "Pinus_maximinoi",] # duplicates and NAs are already removed in this data set, absence are all other species except the modeld one
unique(abs$SPECIES)
nrow(abs)

# Randomly select 500 from these points
set.seed(1734)
samp <- sample(nrow(abs), 500)
abs <- abs[samp,]
head(abs)
abs$SPECIES <- 0
head(abs)

# Combine presence and absence data
Pm.abs <- rbind(Pinus.maximinoi,abs)
View(Pm.abs) # just to check


#####################################################################
######################### EXERCISE PART B ###########################
#####################################################################
#######absence points for Bursera simaruba########
# 4) Generate 10,000 random absence points in Mexico.
BS_mask <- modis[[1]] #extent for the random points
set.seed(1963)  #1963 as predefined function --> every person will get the same randompoints with the same seed
BS_abs <- randomPoints(BS_mask, 10000)
par(mfrow=c(1,1))
plot(BS_abs)
head(BS_abs)

#add columns for species
BS_abs <- cbind(BS_abs, 0)
colnames(BS_abs)<-c("SPECIES", "LONGITUDE", "LATITUDE")
head(BS_abs)

bursera_simaruba.shp$SPECIES <-1


# 5) Combine the presence and absence points in 1 dataframe (called "B.sim". (set the column SPECIES to "0" for absence and to "1" for presence).
B.sim <- rbind(BS.climate,BS_abs)
View(B.sim) # just to check


# 6) Extract the values at the presence/absence localities in B. sim from the MODIS remote sensing layers, remove rows with NA values. 

xy <- cbind(BS.climate$LONGITUDE, BS.climate$LATITUDE)
BS.sp <- SpatialPoints(xy)
env <- extract(modis, BS.sp)
# Note: 
# Since B.sim is a (non-spatial) data frame, use the following lines:
#xy <- cbind(BS.climate$LONGITUDE, BS.climate$LATITUDE)
#BS.sp <- SpatialPoints(xy)
#env <- extract(modis, BS.sp)

############################# END OF PART B #########################


#####################################################################
##################### Species Distribution Model ####################
#####################################################################

# We start with a simple model, using the Bioclim algorithm (based on presence-only data)
bc <- bioclim(Pm.abs[Pm.abs$SPECIES == 1,selected]) # Bioclim is a presence-only method (implemented function), so we select only the presence records (and only the selected env. variables)
class(bc)
bc

# Different modeling methods return different type of 'model' objects (typically they have the same name as the modeling method used). All of these 'model' objects, irrespective of their exact class, can be used to with the 'predict' function to make predictions for any combination of values of the independent variables. This is illustrated in the example below where we make predictions with the bioclim model 'bc', for three records with values for variables bio15, bio18, bio19, bio2 and bio7 (the variables used in the example above to create the model objects).
#values in c are in this case random values for the BIOs
bio15 = c(90, 110, 87)
bio18 = c(470, 555, 612)
bio19 = c(45, 80, 23)
bio2 = c(140, 152, 128)
pd <- data.frame(cbind(bio15, bio18, bio19, bio2))
pd

predict <- predict(bc, pd)
predict
# Making such predictions for a few environments can be very useful to explore and understand model predictions. For example it can used in the response function that creates response plots for each variable, with the other variables at their median value.

response(bc)

# In most cases, however, the purpose is SDM is to create a map of suitability scores. We can do that by providing the predict function with a Raster* object and a model object. As long as the variable names in the model object are available as layers (layerNames) in the Raster* object.

p <- predict(climate, bc)
plot(p)


#####################################################################
######################### Model evaluation ##########################
#####################################################################

# First of all, useful questions for model evaluation include:
# 1) Does the model seem sensible, ecologically?
# 2) Do the fitted functions (the shapes of the modeled relationships) make sense?
# 3) Do the predictions seem reasonable? (map them, and think about them)?

# Most modelers use cross-validation. This consists of creating a model with one 'training' data set, and testing it with another data set of known occurrences. Typically, training and testing data are created through random sampling (without replacement) from a single data set. Only in a few cases, training and test data are from dfferent sources.

# We will here divide our species data in two random sets, one for training a Bioclim model (75% of the records), and one for evaluating the model (25% of the records).

samp <- sample(nrow(Pinus.maximinoi), round(0.75 * nrow(Pinus.maximinoi)))
samp
traindata.pres <- Pinus.maximinoi[samp,]
nrow(traindata.pres)
testdata.pres <- Pinus.maximinoi[-samp,]
nrow(testdata.pres)

# Again, we are running the Bioclim model, but only based on the species data selected for training
bc <- bioclim(traindata.pres[,selected]) # choose only the selected environmental variables

# For model evaluation, we always need some absence data, also for the methods that are based on presence-only algorithms
ebc <- evaluate(testdata.pres, abs, bc)
class(ebc)
ebc
str(ebc)  

# You can use the @ sign to address different slots of this S4 object
ebc@auc     #auc=area under curve; auc max = 1 (=the best)

plot(ebc, 'ROC')

# We can also extract values for the "maximum of the sum of the sensitivity (true positive rate) and specificity (true negative rate)" (this is sometimes uses as a threshold for setting cells to presence or absence).

tr <- threshold(ebc, 'spec_sens')   #balanced threshold between true-positive and true-negative values
tr

# And we use the RasterStack with predictor variables to make a prediction to a RasterLayer:
pb <- predict(climate, bc)
pb

# Make maps of the continuous prediction and the binary presence/absence map
par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(study.area.shp, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence', legend=FALSE, col=c("antiquewhite1","green4"))
plot(study.area.shp, add=TRUE, border='dark grey')
points(Pinus.shp[Pinus.shp$SPECIES=="Pinus_maximinoi",], pch='+')
legend("bottomleft", c("Absence", "Presence"), fill=c("antiquewhite1","green4"))

# Compute the confusion matrix/contingency table
pb.bin <- pb > tr
plot(pb.bin)
pb.bin # This is a binary raster layer

# Extract values at species presence/absence points
xy <- cbind(Pm.abs$LONGITUDE, Pm.abs$LATITUDE)
Pm.abs.sp <- SpatialPoints(xy)
modeled <- extract(pb.bin, Pm.abs.sp)
head(modeled)
s <- cbind(Pm.abs,modeled)
View(s)
nrow(s)

# True positives
true.pos <- s[s$SPECIES == "1" & s$modeled == "1",]
nrow(true.pos)

# True negatives
true.neg <- s[s$SPECIES == "0" & s$modeled == "0",]
nrow(true.neg)

# False positives
false.pos <- s[s$SPECIES == "0" & s$modeled == "1",]
nrow(false.pos)

# False negatives
false.neg <- s[s$SPECIES == "1" & s$modeled == "0",]
nrow(false.neg)

n.total <- nrow(true.pos) + nrow(true.neg) + nrow(false.pos) + nrow(false.neg)
n.total

# Accuracy = (true positive + true negative) / total
(nrow(true.pos) + nrow(true.neg)) / n.total

#---------------------------------------
# In real projects, you would want to use k-fold data partitioning instead of a single random sample. The dismo function 'kfold' facilitates that type of data partitioning. It creates a vector that assigns each row in the data matrix to a group (between 1 and k).

# For k=5
k <- 5
group <- kfold(Pinus.maximinoi, k)
group #all recorrds get one value of the groups, 5 groups because of k
unique(group)

# Now we can fit and test our model five times. In each run, the records corresponding to one of the five groups is only used to evaluate the model, while the other four groups are only used to fit the model. The results are stored in a list called 'e'.

e <- list()
for (i in 1:k) {
        train <- Pinus.maximinoi[group != i,]
        test <- Pinus.maximinoi[group == i,]
        bc <- bioclim(train[,selected])
        e[[i]] <- evaluate(p=test, a=abs, bc)  # using the same absence data as above
        }
# We can extract several things from the objects in 'e', but let's restrict ourselves to the AUC values.

# AUC
auc <- sapply(e, function(x){slot(x, 'auc')} )
auc
mean(auc)


#####################################################################
######################### EXERCISE PART C ###########################
#####################################################################

# 7) Using B.sim, create random subsets of the species presence data for training (75% of data) and testing (25% of data).
# 8) Implement a Bioclim species distribution model for Bursera simaruba (since Bioclim is a presence-only method, you can use all absence data for testing) using all MODIS data.
# 9) Evaluate the model based on AUC, using 5-fold data partitioning.


######################### END OF PART C #############################


#####################################################################
######################### Another model algorithm ###################
#####################################################################

# The Random Forest (Breiman, 2001b) method is an extension of Classification and regression trees (CART; Breiman et al., 1984). In R it is implemented in the function 'randomForest' in a package with the same name. The function randomForest can take a formula or, in two separate arguments, a data.frame with the predictor variables, and a vector with the response.

# We will here divide our species data in two random sets, one for training a Bioclim model (75% of the records), and one for evaluating the model (25% of the records).

# RandomForest requires presence/absence data, so we need to generate a training data set for the absence data as well (see above)
samp <- sample(nrow(abs), round(0.75 * nrow(abs)))
samp
traindata.abs <- abs[samp,]
nrow(traindata.abs)
testdata.abs <- abs[-samp,]
nrow(testdata.abs)

# Combine presence training data generated above with the absence training data 
traindata.presabs <- rbind(traindata.pres, traindata.abs)
View(traindata.presabs)
table(traindata.presabs$SPECIES)

model <- SPECIES ~ bio15 + bio18 + bio19 + bio2 # same variables as above
rf1 <- randomForest(model, data=traindata.presabs) # use only training data here

erf <- evaluate(testdata.pres, testdata.abs, rf1) # use only test data here
erf

# Predition based on the model created above
pr <- predict(climate, rf1)

# Plotting variable importance
varImpPlot(rf1)

# Make maps
par(mfrow=c(1,2))
plot(pr, main='Random Forest, regression')
plot(study.area.shp, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(pr > tr, main='presence/absence', legend=FALSE, col=c("antiquewhite1","green4"))
plot(study.area.shp, add=TRUE, border='dark grey')
points(Pinus.shp[Pinus.shp$SPECIES=="Pinus_maximinoi",], pch='+')
legend("bottomleft", c("Absence", "Presence"), fill=c("antiquewhite1","green4"))


#####################################################################
####################### Combining multiple algorithms ###############
#####################################################################

# Rather than relying on a single "best" model, some authors have argued for using many models and applying some sort of model averaging. Below is a very brief example. See the biomod2 package for more advanced methods.
# weighted mean mbetween the AUC values and ranges
# Make a RasterStack of our individual model predictions:
models <- stack(pb, pr)
names(models) <- c("bioclim", "rf")
plot(models)
m <- mean(models)
plot(m, main='average score')

# However, this is a problematic approach as the values predicted by the models are not all on the same (between 0 and 1) scale; so you may want to fix that first. Another concern could be weighting. Let's combine the two models weighted by their AUC scores. Here, to create the weights, we substract 0.5 (the random expectation) and square the result to give further weight to higher AUC values.

auc <- sapply(list(ebc, erf), function(x) x@auc)
w <- (auc-0.5)^2    #weighting factor
w
m2 <- weighted.mean(models[[c("bioclim", "rf")]], w)
plot(m2, main='Weighted mean')

