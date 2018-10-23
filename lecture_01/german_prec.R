#prec in germany example
install.packages("raster")
library(raster)

germany <- getData("GADM", country="DEU", level=2)
croatia <- getData("GADM", country="HRV", level=2)
plot(germany)
plot(croatia)

prec <- getData("worldclim", var="prec", res=.5, lon=10, lat=51)
#plot(prec)

# crop prec data to extent of germany
prec_ger1 <- crop(prec, germany)
#spplot(prec_ger1)

prec_hrv <- crop(prec, croatia)

# mask with german boundaries
prec_ger2 <- mask(prec_ger1, germany)
#spplot(prec_ger2)

prec_hrv2 <- mask(prec_hrv, croatia)
spplot(prec_hrv2)

prec_avg_GER <- cellStats(prec_ger2, stat = "mean")
#plot(prec_avg_GER)

prec_HRV <- getData("worldclim", var="prec", res=.5, lon=10, lat=51)