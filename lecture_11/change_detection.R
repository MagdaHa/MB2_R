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
