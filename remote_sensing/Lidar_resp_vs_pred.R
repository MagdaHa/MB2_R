###SCRIPT FOR PLOTTING THE RESPONSE VS PREDICTED VALUES#####
## Author: H.LATIFI

rm(list=ls(all=TRUE))

#this should be changed based on the number of bootstraps used for the model training
N=3

#################
####segmente vs. inventurdaten
setwd("E:\\EUFAR\\lidar_excercise_data\\results\\model_results\\output_fullsamp")

resp <- load ("resp_cl1_lidar.RData")
#resp_rf <- load ("c_pl_rf_resp_vol.RData")

pred_rf <- load ("pred_rf_cl1_lidar.RData")
pred_svm <- load ("pred_svm_cl1_lidar.RData")
pred_gp <- load ("pred_gp_cl1_lidar.RData")
pred_lmStep <- load ("pred_lmStep_cl1_lidar.RData")
pred_BRT <- load ("pred_BRT_cl1_lidar.RData")

#################
#######computing accuracy measures#######
rmse_rf <- vector()
bias_rf <- vector()

for (i2 in 1:N) {

#rmse_rf[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_rf_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / mean(resp_cl1[[i2]]) * 100
#bias_rf[i2] <- ((sum(resp_cl1[[i2]]-pred_rf_cl1[[i2]])/length(resp_cl1[[i2]]))/(mean(resp_cl1[[i2]]))*100)
#bias_rf[i2] <- ((sum(resp_cl1[[i2]]-pred_rf_cl1[[i2]]))/(sum(resp_cl1[[i2]]))*100)


#or do it via the range of reference values instead of their mean
rmse_rf[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_rf_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / (max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) * 100
bias_rf[i2] <- ((sum(resp_cl1[[i2]]-pred_rf_cl1[[i2]])/length(resp_cl1[[i2]]))/(max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) *100)

}

rmse_rf <- mean (rmse_rf)
bias_rf <- mean (bias_rf)





rmse_svm <- vector()
bias_svm <- vector ()

for (i2 in 1:N) {
  
#rmse_svm[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_svm_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / mean(resp_cl1[[i2]]) * 100
#bias_svm[i2] <- ((sum(resp_cl1[[i2]]-pred_svm_cl1[[i2]])/length(resp_cl1[[i2]]))/ 100)
 #bias_svm[i2] <- ((sum(resp_cl1[[i2]]-pred_svm_cl1[[i2]]))/(sum(resp_cl1[[i2]]))*100)
  
  
  #or do it via the range of reference values instead of their mean
rmse_svm[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_svm_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / (max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) * 100
bias_svm[i2] <- ((sum(resp_cl1[[i2]]-pred_svm_cl1[[i2]])/length(resp_cl1[[i2]]))/(max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) *100)
  
}

rmse_svm <- mean (rmse_svm)
bias_svm <- mean (bias_svm) 



rmse_gp <- vector()
bias_gp <- vector ()

for (i2 in 1:N) {
  
 #rmse_gp[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_gp_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / mean(resp_cl1[[i2]]) * 100
 #bias_gp[i2] <- ((sum(resp_cl1[[i2]]-pred_gp_cl1[[i2]])/length(resp_cl1[[i2]]))/ 100)
 #bias_gp[i2] <- ((sum(resp_cl1[[i2]]-pred_gp_cl1[[i2]]))/(sum(resp_cl1[[i2]]))*100)
  
  
  #or do it via the range of reference values instead of their mean
rmse_gp[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_gp_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / (max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) * 100
bias_gp[i2] <- ((sum(resp_cl1[[i2]]-pred_gp_cl1[[i2]])/length(resp_cl1[[i2]]))/(max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) *100)
 
}

rmse_gp <- mean (rmse_gp)
bias_gp <- mean (bias_gp)



rmse_lmStep <- vector()
bias_lmStep <- vector ()

for (i2 in 1:N) {
  
 #rmse_lmStep[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_lmStep_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / mean(resp_cl1[[i2]]) * 100
 #bias_lmStep[i2] <- ((sum(resp_cl1[[i2]]-pred_lmStep_cl1[[i2]])/length(resp_cl1[[i2]]))/ 100)
 #bias_lmStep[i2] <- ((sum(resp_cl1[[i2]]-pred_lmStep_cl1[[i2]]))/(sum(resp_cl1[[i2]]))*100)
  
  
  #or do it via the range of reference values instead of their mean
rmse_lmStep[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_lmStep_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / (max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) * 100
bias_lmStep[i2] <- ((sum(resp_cl1[[i2]]-pred_lmStep_cl1[[i2]])/length(resp_cl1[[i2]]))/(max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) *100)
  
}

rmse_lmStep <- mean (rmse_lmStep)
bias_lmStep <- mean (bias_lmStep)


rmse_BRT <- vector()
bias_BRT <- vector ()


for (i2 in 1:N) {
  
 #rmse_BRT[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_BRT_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / mean(resp_cl1[[i2]]) * 100
 #bias_BRT[i2] <- ((sum(resp_cl1[[i2]]-pred_BRT_cl1[[i2]])/length(resp_cl1[[i2]]))/ 100)
 #bias_BRT[i2] <- ((sum(resp_cl1[[i2]]-pred_BRT_cl1[[i2]]))/(sum(resp_cl1[[i2]]))*100)
  
  
  #or do it via the range of reference values instead of their mean
rmse_BRT[i2] <- sqrt(sum((resp_cl1[[i2]]-pred_BRT_cl1[[i2]])^2)/length(resp_cl1[[i2]])) / (max(resp_cl1[[i2]])-min(resp_cl1[[i2]])) * 100
bias_BRT[i2] <- ((sum(resp_cl1[[i2]]-pred_BRT_cl1[[i2]]))/ (max(resp_cl1[[i2]]) -min(resp_cl1[[i2]])) *100)
  
}

rmse_BRT <- mean (rmse_BRT)
bias_BRT <- mean (bias_BRT)




#plot
png(filename = "volhec_resp_vs_pred_final_via_range.png",
    width = 30, height = 30, units = "cm", pointsize = 8,
    bg = "white", res = 300)

if (interactive()) {
  
  par(bg = "white")           # default is likely to be transparent
  
  split.screen(c(3,2))        # split display into two screens
  
  
  screen(1)              
  
  
  for (i2 in 1:(N-1)) {
    par(new=TRUE)
    plot(resp_cl1[[i2]],pred_rf_cl1[[i2]], ylim=range (resp_cl1), xlim=range (pred_rf_cl1), pch='.', axes=F, xlab="", ylab="")
                 
                 }
                 par(new=TRUE)
                 par(cex=1.4)
                 plot(resp_cl1[[N]],pred_rf_cl1[[N]], ylim=range (resp_cl1), xlim=range (pred_rf_cl1), xlab="Response", ylab = "Predictions", pch='.', main="RF")
                 
                 abline(0, 1, lty=2)
                 
                 legend(x='bottomright', bty = 'n', legend=paste(c('relative RMSE% =',round(rmse_rf,3),'relative bias% =', round(bias_rf,3))))
                 
  
  ##
  
  screen(2)              
  
  for (i2 in 1:(N-1)) {
    par(new=TRUE)
    plot(resp_cl1[[i2]],pred_svm_cl1[[i2]], ylim=range (resp_cl1), xlim=range (pred_svm_cl1), pch='.', axes=F, xlab="", ylab="")
    
  }
  par(new=TRUE)
  par(cex=1.4)
  plot(resp_cl1[[N]],pred_svm_cl1[[N]], ylim=range (resp_cl1), xlim=range (pred_svm_cl1), xlab="Response", ylab = "Predictions", pch='.', main="SVM")
  
  abline(0, 1, lty=2)
  
  legend(x='bottomright', bty = 'n', legend=paste(c('relative RMSE% =',round(rmse_svm,3),'relative bias% =', round(bias_svm,3))))
  
  ###
  screen(3)              
  
  for (i2 in 1:(N-1)) {
    par(new=TRUE)
    plot(resp_cl1[[i2]],pred_gp_cl1[[i2]], ylim=range (resp_cl1), xlim=range (pred_gp_cl1), pch='.', axes=F, xlab="", ylab="")
    
  }
  par(new=TRUE)
  par(cex=1.4)
  plot(resp_cl1[[N]],pred_gp_cl1[[N]], ylim=range (resp_cl1), xlim=range (pred_gp_cl1), xlab="Response", ylab = "Predictions", pch='.', main="GP" )
  
  abline(0, 1, lty=2)
  
  legend(x='bottomright', bty = 'n', legend=paste(c('relative RMSE% =',round(rmse_gp,3),'relative bias% =', round(bias_gp,3))))
  
  ###
  screen(4)              
  
  for (i2 in 1:(N-1)) {
    par(new=TRUE)
    plot(resp_cl1[[i2]],pred_lmStep_cl1[[i2]], ylim=range (resp_cl1), xlim=range(pred_lmStep_cl1), pch='.', axes=F, xlab="", ylab="")
    
  }
  par(new=TRUE)
  par(cex=1.4)
  plot(resp_cl1[[N]],pred_lmStep_cl1[[N]], ylim=range (resp_cl1), xlim=range(pred_lmStep_cl1), xlab="Response", ylab = "Predictions", pch='.', main="LMSTEP" )
  
  abline(0, 1, lty=2)
  
  legend(x='bottomright', bty = 'n', legend=paste(c('relative RMSE% =',round(rmse_lmStep,3),'relative bias% =', round(bias_lmStep,15))))
  
  ###
  screen(5)              
  
  for (i2 in 1:(N-1)) {
    par(new=TRUE)
    plot(resp_cl1[[i2]],pred_BRT_cl1[[i2]], ylim=range (resp_cl1), xlim=range(pred_BRT_cl1), pch='.', axes=F, xlab="", ylab="")
    
  }
  par(new=TRUE)
  par(cex=1.4)
  plot(resp_cl1[[N]],pred_BRT_cl1[[N]], ylim=range (resp_cl1), xlim=range(pred_BRT_cl1), xlab="Response", ylab = "Predictions", pch='.', main="GLMBOOST" )
  
  abline(0, 1, lty=2)
  
  legend(x='bottomright', bty = 'n', legend=paste(c('relative RMSE% =',round(rmse_BRT,3),'relative bias% =', round(bias_BRT,17))))
  
  
  
  
  close.screen(all = TRUE)    # exit split-screen mode
}

dev.off()




##############################END OF SCRIPT####################
######################
###############
