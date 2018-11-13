#############################
####importing vector data####
#############################

getwd()

#importing shape file with rgdal
library(rgdal)
steigerwald_rgdal <- readOGR("C:/02_Studium/02_Master/01_Semester 1/MB2_Introduction to Programming and Geostatistics_Wegmann/MB2_R/lecture_04/samplepoint_with_data.shp")
summary(steigerwald_rgdal)

#-----------------------------------------------------------------------------------
#importing shape file with sf
library(sf)
steigerwald_sf <- st_read("C:/02_Studium/02_Master/01_Semester 1/MB2_Introduction to Programming and Geostatistics_Wegmann/MB2_R/lecture_04/samplepoint_with_data.shp")
str(steigerwald_sf)
