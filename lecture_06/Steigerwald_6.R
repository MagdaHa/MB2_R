########################
##Steigerwald analysis##
########################
#-----------------------------------------------------------------------------------------------------------------------
######################################################SESSION02########################################################
#install.packages("RCurl")
library(RCurl)

#load csv dataset fromm GitHum into R
##link kopieren -->view raw dr?cken in github!!##
df <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
#-------------------------------------------------------------------------------------
#basic analysis
head(df)
tail(df)
summary(df)
plot(df)
str(df)
names(df)
dim(df)
class(df)
levels(df)
#select one column
df$LUCAS_LC
#select all sentinel2 data
df[,3:12]
head(df)
#select the secondlast column
df[,length(df)-1]
#only the first 10 rows
df[1:10,]

#----------------------------------------------------------------------------------------------------------------------
######################################################SESSION03#######################################################
names(df)
#only NDVI > 0.3
ndvi <- df$L7.ndvi>0.3
ndvi
#1. plot only SRTM values >= NDVI = 0.3
srtm <- df[df$L7.ndvi >= 0.3, "SRTM"]
srtm
plot(srtm)

#2. plot ndvi where srtm < 400 and landcover = urban
ndvi <- df[df$SRTM<400 & df$LCname=="urban", "L7.ndvi"]
ndvi
plot(ndvi)

#3. create new daraframe with data ndvi > 0.5
ndvi_0.5 <- df[df$L7.ndvi>0.5, ]
ndvi_0.5
head(ndvi_0.5)

#4. plot LUCAS_LC < 2 or > 5
lc <- df[df$LUCAS_LC < 2 | df$LUCAS_LC > 5, ]
head(lc)

#5. select LCname and srtm where ndvi >= 0.7
lc_srtm <- df[df$L7.ndvi >= 0.7, c("LCname","SRTM")]
lc_srtm
plot(lc_srtm$LCname, lc_srtm$SRTM)

#----------------------------------------------------------------------------------------------------------------------
#####################################################SESSION04#######################################################
#copy csv to spdf.obj
spdf.obj <- df
names(spdf.obj)

#install.packages("sp")
library(sp)
#assign xy coordinates
coordinates(spdf.obj)<- ~x+y #set xy coordinates, column x y are xy coordinates
#assign coordinate system utm WGS84 32N (EPSG code: 32632)
proj4string(spdf.obj) <- CRS("+init=epsg:32632")

#install.packages("rgdal")
library(rgdal)
#save csv with xy coordinates as shapefile
setwd("C:\02_Studium\02_Master\01_Semester 1\MB2_Introduction to Programming and Geostatistics_Wegmann\MB2_R_playground\lecture_03")
writeOGR(spdf.obj, "samplepoint_with_data.shp", driver="ESRI Shapefile", "data")

#----------------------------------------------------------------------------------------------------------------------
#####################################################SESSION06#######################################################
##create plots
library(ggplot2)

ggplot(df, aes(x=L8.ndvi, y=L8.savi))                       #ndvi and savi
#scatterplot
ggplot(df, aes(x=L8.ndvi, y=L8.savi)) + geom_point()        
#------------------------------------------------------------------------
#adding info using colour
ggplot(df, aes(x=L8.ndvi, y=L8.savi, color=SRTM)) + geom_point()  #srtm as color
#smoothed lines
ggplot(df, aes(x=L8.ndvi, y=L8.savi, color=SRTM)) + geom_point() + geom_smooth()
#split into landcover
ggplot(df, aes(x=L8.ndvi, y=L8.savi, color=SRTM)) + geom_point() + geom_smooth() + facet_wrap(~LCname) # ever land vocer in a new plot

#------------------------------------------------------------------------
#3 variables --> x = landcover, y= savi, color = srtm
ggplot() +
  geom_point(data = df, aes(LCname, L8.savi, color=SRTM))

#boxplot with jitter
ggplot(df, aes(x=LCname, y=L8.savi))+
  geom_boxplot(alpha=.5)

#boxplot + colorramp
ggplot(df, aes(x=LCname, y=L8.savi))+
  geom_boxplot(alpha=.5)+
  geom_point(aes(color=SRTM), alpha=.7, size=1.5, position=position_jitter(width = .25, height = 0))
  
#geometric violin
ggplot(df, aes(x=LCname, y=L8.savi))+
  geom_violin()

#geometric density
ggplot(df, aes(x=TimeScan.NDVIavg, fill=LCname))+
  geom_density(alpha=0.3)

#jitter + boxplot + colorramp
ggplot(df, aes(x=LCname, y=L8.savi))+
  geom_jitter(aes(alpha=SRTM, size=TimeScan.NDVIsd, color=L8.ndvi))+
  geom_boxplot(alpha=.5)

#------------------------------------------------------------------------
#create cover image
ggplot(df, aes(x=L8.ndvi, y=L8.savi)) +
  geom_point(aes(color=LCname), size=2) +           #every landcover class in a new diagram, axes savi and ndvi
  facet_grid(.~LCname)
  
ggplot(df, aes(x=L8.ndvi, y=L8.savi)) +
  geom_point(aes(color=LCname, size=SRTM)) +        #srtm as size of landcover dots
  facet_grid(.~LCname)

ggplot(df, aes(x=L8.ndvi, y=L8.savi)) +
  geom_point(aes(color=SRTM), size=2) +             #every landcover class in a new diagram, axes savi and ndvi
  facet_grid(.~LCname)+
  scale_fill_gradient()

#--------------------------------------------------------------------------
#plotting SRTM data
library(RStoolbox)

my_raster <- df
coordinates(my_raster) <- ~x+y
my_raster[] <- df$SRTM

ggR(my_raster$SRTM, geom_raster = T)+
  scale_fill_gradient(low = "lightblue", high = "darkblue", name="elevation", na.vakue=NA)+
  labs(x="", y="")+
  ggtitle("SRTM")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y=element_text(angle = 45, size = 6))+
  scale_y_continuous(breaks = seq(5527000, 5538000, 4000))+
  xlab("")+
  ylab("")
