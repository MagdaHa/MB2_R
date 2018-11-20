#############
####ggmap####
#############

#install.packages("ggmap")
#install.packages("mapproj")

library(ggplot2)
library(ggmap)

library(maps)
library(mapproj)
library(ggalt)

#------------------------------------------------------------------------------------
#get data for defined location
map.wue <- get_map("Wurzburg", source = "osm")

#plot map of this location
ggmap(map.wue)

#zoom in
ggmap(map.wue, zoom=15)

#overview map
map <- get_map("Bavaria", zoom = 6)
ggmap(map)

wue <- geocode("Wurzburg")
##############################DOESNOTWORK############################################










