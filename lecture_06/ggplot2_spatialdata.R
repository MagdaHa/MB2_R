################################
####ggplot 2 for spatial data####
################################


#loading random landsat image
library(RStoolbox)
data(lsat)
lsat.df <- data.frame(coordinates(lsat), getValues(lsat))
lsat.df <- lsat.df[lsat.df$B3_dn !=0,]

#--------------------------------------------------------------------------------------
#plotting lsat image with color scales
library(gglpot2)

ggplot(lsat.df) +
  geom_raster(aes(x=x, y=y, fill=B4_dn)) +
  scale_fill_gradient(low="blue", high="yellow", na.value=NA) +
  coord_equal()

#-------------------------------------------------------------------------------------
#RGB plotting with predefined band combination
plotRGB(lsat, 3,2,1)
plotRGB(lsat, 4,2,1, stretch= "lin")                              #4=IR, linear stretch

#--------------------------------------------
##or with ggplot2
#linear stretched
ggRGB(lsat, 4,2,1, stretch = "lin")
ggR(lsat, layer = 4, maxpixels = 1e6, stretch = "hist")

#single layer with coloured legend
ggR(lsat, layer=1, stretch = "lin", geom_raster = TRUE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

#--------------------------------------------
#storing plot in variable a
a <- ggplot(lsat.df) +
  geom_raster(aes(x=x, y=y, fill=B2_dn))+
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = NA) + coord_equal()
a

#spatial vector from RStoolbox
poly <- readRDS(system.file("external/trainingPolygons.rds"), package="RStoolbox")    #???????????????????????????????
plots <- as.data.frame(coordinates(poly))

a+guides(fill=guide_colorbar()) + geom_point(data=plots, aes(x=V1, y=V2), shape=3, color="yellow") +
  theme(axis.title.x = element_blank())

#--------------------------------------------------------------------------------------------------

