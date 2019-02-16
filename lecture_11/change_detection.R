########################
####change detection####
########################

##change vector analysis
library(RStoolbox)
library(raster)
NDWI_0418 <- raster("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI\\NDWI_10042018.tif")
NDWI_0718 <- raster("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\NDWI\\NDWI_31072018.tif")
par(mfrow=c(1,2))
plot(NDWI_0418)
plot(NDWI_0718)

#crop image -> https://www.rdocumentation.org/packages/OpenImageR/versions/1.1.3/topics/cropImage
extent(NDWI_0718)
e <- extent(650000, 750000, -2130000, -2040000)
NDWI_0418_crop <- crop (NDWI_0418, extent(e))
NDWI_0718_crop <- crop (NDWI_0718, extent(e))
plot(NDWI_0418_crop)
plot(NDWI_0718_crop)


NDWI_0418.sd <- focal(NDWI_0418_crop,w=matrix(1/9,nrow=3,ncol=3),fun=sd) #Umgebunspixel von 3x3 werden ermittelt
NDWI_0718.sd <- focal(NDWI_0718_crop,w=matrix(1/9,nrow=3,ncol=3),fun=sd)
plot(NDWI_0418.sd)
plot(NDWI_0718.sd)


stack_0418.2 <- stack(NDWI_0418_crop, NDWI_0418.sd)  #zwei Bänder werden künstlich erzeugt
stack_0718.2 <- stack(NDWI_0718_crop, NDWI_0718.sd)
#writeRaster(stack_0718.2, filename = "test", format="GTiff",overwrite=TRUE)
x11()
plot(stack_0718.2)
getwd()

cv_2018 <- rasterCVA(stack_0418.2 [[1:2]], stack_0718.2 [[1:2]])
x11()
plot(cv_2018)


##minus calculatin of two images
change_18 <- NDWI_0718_crop - NDWI_0418_crop # or vise versa
plot(change_18)




library(gganimate)
# Only show constructed wind turbines
REPD_wind <-
  REPD %>%
  filter(`Technology Type` == "Wind Onshore") %>%
  filter(Status.Summary == "Approved")

# Create gif
invisible(saveGIF({
  for (i in 1990:2017){
    
    p <- 
      ggplot(data = NULL, aes(x = `X-coordinate`, y = `Y-coordinate`)) +
      geom_polygon(data = UK, aes(long, lat, group = group), fill = "#528076") +
      geom_point(data = filter(REPD_wind, year < i), 
                 fill = "#b2d1e0", size = 3, alpha = 0.8, shape = 21) +
      geom_point(data = filter(REPD_wind, year == i), 
                 fill = "gold", size = 5, shape = 21) +
      coord_equal(xlim = c(-150000, 900000), ylim = c(0, 1200000)) +
      labs(title = "Location of Installed Projects") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = "grey99", colour = "grey80"),
            plot.title = element_text(hjust = 0.5))
    
    p2 <- 
      ggplot(filter(REPD_wind, year <= i), aes(x = year, y = `Installed Capacity (MWelec)`)) +
      geom_col(aes(fill= year >= i)) +
      scale_fill_manual(values = c("#b2d1e0","gold")) +
      scale_x_continuous(limits = c(1990, 2018), expand = c(0,0)) +
      scale_y_continuous(limits = c(0, 2000), expand = c(0,0)) +
      labs(title = "Annual Installed Capacity", x = "Year", y = "Installed Capacity (MW)") +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "grey99", colour = "grey80"),
            plot.title = element_text(hjust = 0.5))
    
    
    p3 <- ggplot(data = NULL, aes(x = 1990:2017, y = 1)) +
      geom_line() +
      geom_point(aes(fill = (x = 1990:2017 > i)), shape = 21, size = 5) +
      theme_void() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("#b2d1e0","gold")) +
      geom_text(aes(x = i, y = 1, label = i), vjust = -1, size = 9) +
      theme(panel.background = element_rect(fill = "grey99", colour = "grey80"))
    
    
    
    # Print plots using patchwork
    print(p + p2 - p3 + plot_layout(ncol = 1, heights = c(5, 1)))
  }
  
  
}, movie.name = "windDevelopment.gif", interval = 1, ani.width = 1000, ani.height = 700))