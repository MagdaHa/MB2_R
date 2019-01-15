###################################
#### supervised classification ####
###################################

####training samples as input needed####

library(RStoolbox)
library(rgdal)

#loading satellite image to clssify
image <- brick("C:\\02_Studium\\02_Master\\01_Semester 1\\MB1_Digital Image Analysis and GIS_Hüttich\\04_data\\Steigerwald\\03_raster\\01_landsat\\02_timescan\\TimeScan_EAGLE_AOI_UTM_WGS84.tif")
#plot(image)

#loading training data
td <- rgdal::readOGR("C:\\02_Studium\\02_Master\\01_Semester 1\\MB1_Digital Image Analysis and GIS_Hüttich\\01_Uebungen\\classification_steigerwald\\TimeScan_trainingsamples.shp")
#plot(td)

#create supervised classification, save as tif
sc <- superClass(image, trainData = td, responseCol = "Classvalue", filename = "myClassification.tif")  #responseCol: name of the column where samples are stored
plot(sc$map)


#accuracy assessment
validateMap(sc$map, mode = "classification")
rasterEntropy(image)

aa1 <- superClass(image, trainData=td, responseCol = "Classvalue", trainPartition = 0.7)    # training classes don't have to be merged!
a1$calidation$performance