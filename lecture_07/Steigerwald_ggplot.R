#########################################################
####Steigerwald: visualizing raster data with ggplot2####
#########################################################

library(raster)
library(sp)
library(ggplot2)
library(RStoolbox)
library(tmap)
library(tmaptools)


#----------------------------------------------------------------------------------------------------------------------------
dgm_srtm <- raster("C:\\02_Studium\\02_Master\\01_Semester 1\\MB1_Digital Image Analysis and GIS_Hüttich\\04_data\\Steigerwald\\03_raster\\05_srtm_1arc\\SRTM.tif")
plot(dgm_srtm)

head(dgm_srtm)
names(dgm_srtm)

#--------------------------------------------------------------------------------------------------------------------
#get data for a defined location
cities <- geocode_OSM(c("FabrikSchleichach", "Rauhenebrach"),projection = crs(dgm_srtm))  #city locations via osm
cities_df <- data.frame(cities)                                                           #dataframe with predefined cities
head(cities_df)
#convert it to spatial objects
sp::coordinates(cities) <- ~x+y                                                           #coordinates

#---------------------------------------------------------------------------------------------------------------------
ggR(dgm_srtm, geom_raster = T)+
  scale_fill_gradient(low = "lightblue", high = "darkblue", name="elevation in m", na.value=NA)+
  labs(x="", y="")+
  ggtitle("Elevation model of Steigerwald - SRTM")+                             #title name
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))+      #title size, specification
  theme(legend.title = element_text(size = 10, face = "bold"))+                 #legend title size, specification
  theme(legend.text = element_text(size = 6))+                                  #legend element size
  theme(axis.text.y=element_text(angle = 45, size = 9))+                        #y axis text with 45°angle
  xlab("longitude")+                                                            #x axis title
  ylab("latitude")+                                                             #y axis title
  theme_bw()   +                                                                #background color
  geom_point(data=cities_df, aes(x=x, y=y), color="red", fill="green", alpha=.5, size=2)+ #city locations
  geom_text(data = cities_df, aes(x=x, y=y, label=query),size=2, nudge_x = 5, nudge_y = 5, show.legend = T)+ #city names
