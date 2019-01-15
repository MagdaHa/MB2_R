######SCRIPT FOR PLOTTING THE WTW MAPS OF FOREST STRUCTURAL ATTRIBUTES####
## Author: H.LATIFI

rm(list=ls(all=TRUE))

setwd("E:\\EUFAR\\lidar_excercise_data\\results\\model_results\\output_fullsamp")

##load the data, unlist it, stack it and calculate the average of all runs
fullpred <- load ("fullpred_cl1_rf.RData")

fullpred_unl <- unlist(fullpred_cl1_rf)

fullpred_unl_stack <- stack (fullpred_unl)

fullpred_mean <- mean (fullpred_unl_stack)

#coefficient of variation
co.var <- function(x) ( 100*sd(x)/mean(x) )

beginCluster(type="SOCK",n=2)# n defines the number of cores to use 

result_covar<-clusterR(fullpred_unl_stack, calc, args=list(fun = co.var))

endCluster()# end the multicore-cluster 

################################################

fullpred_mean_write <- writeRaster (fullpred_mean, "fullpred_rf_mean.tif", overwrite=T)
fullpred_cv_write <- writeRaster (result_covar, "fullpred_rf_cv.tif", overwrite=T)

#a <- raster ("fullpred_rf_mean.tif")
#x11(); plot (a)


 

         png("map_rf_covar.tiff",
              width=20, height=20,units="cm", bg="white",res = 300)

         plot(result_covar, main="",legend=T)
         northarrow(c(0.85, 0.85), 0.07, coordsystem="npc", cex=0.7)

         dev.off()


          png("map_rf_covar_BW.tiff",
              width=20, height=20,units="cm", bg="white",res = 300)

         plot(result_covar, main="",legend=T,col=grey(500:0/500))
         northarrow(c(0.85, 0.85), 0.07, coordsystem="npc", cex=0.7)

         dev.off()

         
 #####################################
###END OF SCRIPT######