#####################
##creating a raster##
#####################
#creating a dataframe with random values
df <- data.frame(measure1=runif(1000)*100, measure2=round(rnorm(1000)*100))
length(df$measure1)
#------------------------------------------------------

#creating an empty raster dataset r1
library(raster)
r1 <- raster(nrows=10, ncols=10)
#filling empty raster with 100 random values
r1[] <- rnorm(100) # [] uses r1 and replaces the empty raster with random data
r1
plot(r1)
#or
r1[] <- df$measure1[1:100]
plot(r1)

#create a second raster dataset r2
r2 <- raster(nrows=10, ncols=10)
#filling empty raster with 100 random values
r2[] <- rnorm(100) # [] uses r1 and replaces the empty raster with random data
r1
plot(r2)
#or
r2[] <- df$measure2[1:100]
plot(r2)

#stack both raster datasets together (like bands on a satellite image)
r12 <- stack(r1, r2)
r12
plot(r12)
#plot only the first raster of the stack
plot(r12[[1]])

#new layer into raster object
r12$new <- r12[[1]]*r12[[2]]^2 # new layer = pixel layer 1 * pixel layer 2 ^2
plot(r12)

#raster vonterting into data frame
df12 <- r12[] # [] only the data in the raster, not the raster itself
head(df12)


#-----------------------------------------------------------------------------------
##raster calculation
#arithmetic
raster12 <- r1*r2
plot(raster12)

#complex functions
raster_sd <- calc(raster12, fun=sd)
plot(raster_sd)

#extended options???????????????????????????
raster_sd2 <- calc(raster12, fun=sd, filename="raster_sd2.tif", options=c("COMPRESS=DEFLATE"))

#regression analysis
raster12_1 <- stack(r1, r2, fun=function())

#----------------------------------------------------------------------------------
##Landsat information, examples
#install.packages("RStoolbox")
library(RStoolbox)
data(lsat) #Landsat
str(lsat)

#return first band
lsat[[1]]
#or
lsat$B1_dn

#plot first band
plot(lsat[[1]])

#copy 2nd and 3rd band into a new object
x <- lsat[[2:3]]

#values of rows 1 to 10 of each band matrix
x2 <- lsat[1:10, ]

#set all values < 0 to NA
lsat[lsat<0] <- NA

#set values 8 and 7 to -9999
lsat[lsat %in% c(7,8)] <- -9999

#----------------------------
# adding own function????????????
data(lsat)
scaleFactor <- 1000
fun <- function(x) {
  (x - rowMeans(x)) * scaleFactor
}
raster_output <- calc(raster12, fun, forcefun = TRUE, filename = "myStandardizedAndScaled.tif", dataType="INT2S")

#------------------------------------------------------------------------------------
##reclassification
m <- c(0, 0.25, 1, 0.25, 0.5, 2, 0.5, 1, 3)
conversionMatrix <- matrix(m, ncol=3, byrow=TRUE)
conversionMatrix

