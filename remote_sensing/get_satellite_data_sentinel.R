##################################
#### Download Sentinel Images ####
##################################
#-----------------------------------https://github.com/16EAGLE/getSpatialData-----------------------------------#

# to download an R package from a GitHub, devtools are required
# install.packages(devtools)
devtools::install_github("16EAGLE/getSpatialData", force=TRUE)

#import used packages
library(getSpatialData)
library(raster)
library(sf)
library(sp)

#--------------------------------------------------------------------------------------------------------------------------
# Setting an area of interest within agraphical user interface
set_aoi()
#anchoring AOI
view_aoi()

#--------------------------------------------------------------------------------------------------------------------------
# Log in into your Copernicus Hub account
login_CopHub(username = "magdalenaha") #asks for password or define 'password

#--------------------------------------------------------------------------------------------------------------------------
# Specify a time range and the sattelite platform
time_range <-  c("2018-07-01", "2018-10-30")
platform <- "Sentinel-2" #or "Sentinel-1" or "Sentinel-3"

#--------------------------------------------------------------------------------------------------------------------------
# specify an output directory for downloaded scenes
set_archive("C:/02_Studium/02_Master")

#--------------------------------------------------------------------------------------------------------------------------
# Querying the Sentinel Hub
records <- getSentinel_query(time_range = time_range, platform = platform)
colnames(records)

# FIlter the found scene due to cloud coverage and product level
records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
records_filtered <- records_filtered[as.numeric(records_filtered$cloudcoverpercentage) <= 10, ] #filter by clouds

#--------------------------------------------------------------------------------------------------------------------------
# View records table
View(records)
View(records_filtered)

# show a preview of  specific scene
getSentinel_preview(record = records_filtered[3,])

#--------------------------------------------------------------------------------------------------------------------------
# Actually downlod a list of scenes
datasets <- getSentinel_data(records = records_filtered[c(1,3), ])

#--------------------------------------------------------------------------------------------------------------------------
# Finally, define an output format and make them ready-to-use
datasets_prep <- prepSentinel(datasets, format = "tiff")
# or use VRT to not store duplicates of different formats
datasets_prep <- prepSentinel(datasets, format = "vrt")

#--------------------------------------------------------------------------------------------------------------------------
#View the outpt files
datasets_prep[[1]][[1]][1] #first dataset, first tile, 10 m resolution
datasets_prep[[1]][[1]][2] #first dataset, first tile, 20 m resolution
datasets_prep[[1]][[1]][3] #first dataset, first tile, 60 m resolution

