###########################
####Homework lecture 05####
###########################

#Loading libraries
#install.packages("rgdal")
library("rgdal")
library("rgeos")
library("sp")

#----------------------------------------------------------------------------------------------------
#set Working Directory
setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\MB2_R\\lecture_05")

#loading data
Census.Data <- read.csv("practical_data.csv")
names(Census.Data)

#loading output area shapefile
Output.Area <- readOGR("Camden_oa11.shp")

#loading house point files
House.Point = readOGR("Camden_house_sales.shp")

#---------------------------------------------------------------------------------------------------
#joining Census data to the shapefile
OA.Census <- merge(Output.Area, Census.Data, by.x = "OA11CD", by.y = "OA")

#projecting the coordinate system
proj4string(OA.Census) <- CRS("+init=EPSG:27700")
proj4string(House.Point) <- CRS("+init=EPSG:27700")

#------------------------------------------------------------------------------------------------
##connecting polygon attributes to points##
#asssigning each house point the characteristics of the output areas --> point in polygon
pip <- over(House.Point, OA.Census)
#binding the census data to the original points
House.Point@data <- cbind(House.Point@data, pip)
View(House.Point@data)
#plot of house prices and local umemployed rates
plot(log(House.Point@data$Price), House.Point@data$Unemployed)

##measuring of the average house price for each area##
#mean for each OA
OA <- aggregate(House.Point@data$Price, by = list(House.Point@data$OA11CD), mean)
#changing column names of the aggregate data
names(OA) <- c("OA11CD", "Price")
#joining back the aggregated data to the OA.Census polygon
OA.Census@data <- merge(OA.Census@data, OA, by = "OA11CD", all.x = TRUE)

final.data <- merge(OA.Census, data, by="")
View(final.data)

#--------------------------------------------------------------------------------------------------
#mapping the data
#install.packages("tmap")
library(tmap)
tm_shape(OA.Census) + tm_fill(col = "Price", style = "quantile", title = "Mean House Price (Pound)")

#-----------------------------------------------------------------------------------------------------
#scatter plot with ggplot2
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point()
#scatter plot with good visualization
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point(aes(colour = White_British, size = Low_Occupancy))+
  labs(title="Relationship between age and unemployment in the UK")
 

