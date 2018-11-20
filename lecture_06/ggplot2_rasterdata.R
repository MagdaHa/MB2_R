############################################
####visualizing raster data with ggplot2####
############################################

library(raster)
dgm <- raster("C:/Bachelorarbeit/Aster GDEM/Arbeitsgebiet_clip.tif")
plot(dgm)

head(dgm)
names(dgm)


library(ggplot2)
library(RStoolbox)

ggR(dgm, geom_raster = T)+
  scale_fill_gradient(low = "lightblue", high = "darkblue", name="elevation in m", na.value=NA)+
  labs(x="", y="")+
  ggtitle("ASTER GDEM of the area near Nazca, Peru")+                           #title name
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))+      #title size, specification
  theme(legend.title = element_text(size = 10, face = "bold"))+                 #legend title size, specification
  theme(legend.text = element_text(size = 6))+                                  #legend element size
  theme(axis.text.y=element_text(angle = 45, size = 9))+                        #y axis text with 45°angle
  xlab("latitude")+                                                                #x axis title
  ylab("longitude")+                                                                     #y axis title
  theme_bw()                                                                    #background color

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#optional scale fill color models#-#-#-#-#-#
#scale_fill_continuous(type="viridis")+
#scale_color_gradientn(colours = terrain.colors(7))+
#scale_color_distiller(palette = "RdPu")+
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-