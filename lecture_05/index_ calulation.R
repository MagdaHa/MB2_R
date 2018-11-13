##########################
####index calculations####
##########################

#----------------------------------------------
##NDVI

library(raster)
setwd("C:/02_Studium/02_Master/01_Semester 1/MB1_Digital Image Analysis and GIS_Hüttich/S2B_MSIL1C_20180530T103019_N0206_R108_T32UNA_20180530T123402.SAFE/GRANULE/L1C_T32UNA_A006426_20180530T103018/IMG_DATA")
red_b <- raster("T32UNA_20180530T103019_B03.jp2")
plot(red_b)

nir_b <- raster("T32UNA_20180530T103019_B04.jp2")
plot(nir_b)

ndvi <- (nir_b - red_b)/(nir_b + red_b)
plot(ndvi)


#---------------------------------------------
##MSAVI with landsat example data

library(RStoolbox)
data(lsat)

savi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices ="MSAVI")
plot(savi)


#---------------------------------------------
##calculating all indices only with red and nir band with landsat example data

library(RStoolbox)
data(lsat)

red_nir <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn") #indices without definition -> calculats all indices with red and nir bands
plot(red_nir)

red_nir.sd <- calc(red_nir, fun = sd)                              #calculating standard deviation of VIs indices
plot(red_nir.sd)
