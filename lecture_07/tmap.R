########################
####tmap - geocoding####
########################

library(tmap)
library(tmaptools)

#------------------------------------------------------------------------------
#get data for a defined location
cities1 <- geocode_OSM(c("Würzburg", "Dhaka", "Sydney", "New York"))
cities2 <- geocode_OSM(c("Cape Town", "Lagos", "Buenos Aires", "Gerbrunn"))

#------------------------------------------------------------------------------
#convert it to spatial objects
sp::coordinates(cities1) <- ~lon+lat
sp::coordinates(cities2) <- ~lon+lat

#------------------------------------------------------------------------------
#plotting the locations
current.mode <- tmap_mode("view")

tm_shape(cities1)+
  tm_dots(col="blue")+
  tm_shape(cities2)+
  tm_dots(col="red")









