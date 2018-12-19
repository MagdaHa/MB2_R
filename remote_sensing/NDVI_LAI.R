##################################
#### NDVI and LAI calculation ####
##################################

library(raster)

#importing Landsat image
landsat_176 <- brick("C:\\02_Studium\\02_Master\\01_Semester 1\\MB1_Digital Image Analysis and GIS_Hüttich\\03_codes\\LAI\\starfm_p_176_from_160_and_192.tif")

#----------------------------------------------------------------------------------------------------------
#calculation of NDVI
NDVI <- function(nir,red) {(nir-red)/(nir+red)}
NDVI_176 <- NDVI(landsat_176$starfm_p_176_from_160_and_192.4,landsat_176$starfm_p_176_from_160_and_192.3)
plot(NDVI_176)

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#calculation of LAI direct
LAI_DIR<-0.57*exp(2.33*NDVI_176)
plot(LAI_DIR)

#------------------------------------------------------------------------------------------------------------
library(rgdal)
#point data, in situ points
WW_points<-readOGR("C:\\02_Studium\\02_Master\\01_Semester 1\\MB1_Digital Image Analysis and GIS_Hüttich\\03_codes\\LAI\\doy_176.shp")
WW_points@coords <- WW_points@coords[, 1:2]
ndvi_extract<-data.frame(extract(NDVI_176,WW_points))
plot(WW_points, add=TRUE)

#------------------------------------------------------------------------------------------------------
#regression line
x<-ndvi_extract
y<-data.frame(WW_points$True_LAI)

df2<-cbind(x,y)

x<-df2$extract.NDVI_176..WW_points.
y<-df2$WW_points.True_LAI

#names(df2)<-c("NDVI","LAI")

plot(x,y,col="darkgreen",pch=16 , cex=1.3,xlab="NDVI", ylab="LAI",main="Scatter Plot")

model=lm(y ~ x ,data = df2)

myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 ) 

# I add the features of the model to the plot
coeff=round(model$coefficients , 2)
mtext(paste( "Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "R^2 = ",round(summary(model)$adj.r.squared,2)), side=3, line=0.5, at=0.7)

#-----------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
##calculation of LAI extrapolated
LAI_Extra<--12.89+(NDVI_176*23.08)
plot(LAI_Extra)

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#LAI direct vs LAI extracted
plot(LAI_Extra, main ="Extrapolation")
plot(LAI_DIR,main="LAI direct")

#--------------------------------------------------------------------------------------------------------------
#scatter plot of LAI direct
LAI_DIR_extract<-data.frame(extract(LAI_DIR,WW_points))

x<-data.frame(WW_points$True_LAI)
y<-data.frame(LAI_DIR_extract)

df3<-cbind(x,y)

x<-df3$WW_points.True_LAI
y<-df3$extract.LAI_DIR..WW_points.

plot(x,y,col="darkgreen",pch=16 , cex=1.3,xlab="LAI Measured", ylab="LAI Perdicted",main="Scatter Plot")

model=lm(y ~ x ,data = df3)

myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 )

# I add the features of the model to the plot
rmse=round(model$coefficients , 2)
coeff=round(model$coefficients , 2)
mtext(paste( "Model : ",coeff[1] , " + " , coeff[2] , "*x ;"  , "RMSE = ",round(sqrt(mean((x-y)^2)),2) ,";","R^2 = ",round(summary(model)$adj.r.squared,2)), side=3, line=0.5, at=4)

#--------------------------------------------------------------------------------------------------------------
#scatter plot of LAI extralopated
LAI_Extra_extract<-data.frame(extract(LAI_Extra,WW_points))

x<-data.frame(WW_points$True_LAI)
y<-data.frame(LAI_Extra_extract)

df4<-cbind(x,y)

x<-df4$WW_points.True_LAI
y<-df4$extract.LAI_Extra..WW_points.

plot(x,y,col="darkgreen",pch=16 , cex=1.3,xlab="LAI Measured", ylab="LAI Perdicted",main="Scatter Plot")

model=lm(y ~ x ,data = df4)

myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 ) 

# I add the features of the model to the plot
rmse=round(model$coefficients , 2)
coeff=round(model$coefficients , 2)
mtext(paste( "Model : ",coeff[1] , " + " , coeff[2] , "*x ;"  ,"RMSE = ",round(sqrt(mean((x-y)^2)),2) ,";", "R^2 = ",round(summary(model)$adj.r.squared,2)), side=3, line=0.5, at=4)
