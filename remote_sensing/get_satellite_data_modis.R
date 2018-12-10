###############################
#### Download MODIS Images ####
###############################
#-----------------------------------https://github.com/16EAGLE/getSpatialData-----------------------------------#
#install.packages("devtools")
library(devtools)
devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
#install.packages("raster")
library(raster)
library(sp)

#---------------------------------------------------------------------------------------------------------------
#setting AOI in window
set_aoi()
#anchoring AOI
view_aoi()

#----------------------------------------------------------------------------------------------------------------
#displaying time range
time_range <- c("2017-08-01", "2017-08-30")

#----------------------------------------------------------------------------------------------------------------
#login USG
login_USGS("Magdalena1") # password in command line

#setting working directory
set_archive("C:/02_Studium/02_Master")

#----------------------------------------------------------------------------------------------------------------
#get MODIS data
product_names <- getMODIS_names()
product <- grep("MOD13Q8", product_names, value=T)

query <- getMODIS_query(time_range=time_range, name = product)
getMODIS_preview(query[5,])           #5th and al following images
files <- getMODIS_data(query[1,])     #save images

