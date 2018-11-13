########################
##Steigerwald analysis##
########################
#-----------------------------------------------------------------------------------------------------------------------
######################################################SESSION02########################################################
#install.packages("RCurl")
library(RCurl)

#load csv dataset fromm GitHum into R
##link kopieren -->view raw dr?cken in github!!##
df <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
#-------------------------------------------------------------------------------------
#basic analysis
head(df)
tail(df)
summary(df)
plot(df)
str(df)
names(df)
dim(df)
class(df)
levels(df)
#select one column
df$LUCAS_LC
#select all sentinel2 data
df[,3:12]
head(df)
#select the secondlast column
df[,length(df)-1]
#only the first 10 rows
df[1:10,]

#----------------------------------------------------------------------------------------------------------------------
######################################################SESSION03#######################################################
names(df)
#only NDVI > 0.3
ndvi <- df$L7.ndvi>0.3
ndvi
#1. plot only SRTM values >= NDVI = 0.3
srtm <- df[df$L7.ndvi >= 0.3, "SRTM"]
srtm
plot(srtm)

#2. plot ndvi where srtm < 400 and landcover = urban
ndvi <- df[df$SRTM<400 & df$LCname=="urban", "L7.ndvi"]
ndvi
plot(ndvi)

#3. create new daraframe with data ndvi > 0.5
ndvi_0.5 <- df[df$L7.ndvi>0.5, ]
ndvi_0.5
head(ndvi_0.5)

#4. plot LUCAS_LC < 2 or > 5
lc <- df[df$LUCAS_LC < 2 | df$LUCAS_LC > 5, ]
head(lc)

#5. select LCname and srtm where ndvi >= 0.7
lc_srtm <- df[df$L7.ndvi >= 0.7, c("LCname","SRTM")]
lc_srtm
plot(lc_srtm$LCname, lc_srtm$SRTM)

#----------------------------------------------------------------------------------------------------------------------
#####################################################SESSION04#######################################################
#copy csv to spdf.obj
spdf.obj <- df
names(spdf.obj)

#install.packages("sp")
library(sp)
#assign xy coordinates
coordinates(spdf.obj)<- ~x+y #set xy coordinates, column x y are xy coordinates
#assign coordinate system utm WGS84 32N (EPSG code: 32632)
proj4string(spdf.obj) <- CRS("+init=epsg:32632")

#install.packages("rgdal")
library(rgdal)
#save csv with xy coordinates as shapefile
setwd("C:\02_Studium\02_Master\01_Semester 1\MB2_Introduction to Programming and Geostatistics_Wegmann\MB2_R_playground\lecture_03")
writeOGR(spdf.obj, "samplepoint_with_data.shp", driver="ESRI Shapefile", "data")