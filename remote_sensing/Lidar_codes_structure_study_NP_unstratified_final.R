## R code for modeling a number of forest structural variables
## using LiDAR-derived predictors
## written by H.Latifi and F.Fassnacht
## Example source of application: 
## Latifi, H., Fassnacht, F.E., Hartig, F., Berger, C., Hernandez, J., Corvalan, P., Koch, B. 
##Stratified Aboveground Forest Biomass Estimation by Remote Sensing Data
## Int. J. Appl. Earth Obs. Geoinf.38: 229-241. 



# a fancy function to install and load the needed packages
loadandinstall <- function(mypkg) {if (!is.element(mypkg,
                                                   installed.packages()[,1])){install.packages(mypkg)}; library(mypkg,
                                                                                                                character.only=TRUE) }
print("Loading necessary Libraries")
loadandinstall("raster")
loadandinstall("rgeos")      # for data transformation
loadandinstall("stringr")
loadandinstall("car")
loadandinstall("knitr")      # HTML erstellen arbeiten mit Chuncs
loadandinstall("caret")
#loadandinstall("RStoolbox")  # Erstellt von Daniel und Buch mit noch einem veröffentlicht
loadandinstall("maptools")
loadandinstall("sp"): 
loadandinstall("randomForest")
#loadandinstall("e1071")
loadandinstall("rgdal")

##### 
rm(list=ls(all=TRUE))


wd <- setwd("D:\\Excercises\\FUSION")


##### load data table 

h2.00_hmax <- read.csv2 ("results\\metrics\\metrics_all2.csv", header=T)


hdens <- raster ("results\\densitymetrics\\canopycover.asc")

reference <- read.csv2 ("data\\trans\\final_modelled\\Bioklim_Zenner_1000_2.csv", header=T)

# delete the last rows containing no lidar plots
reference <- reference [- c(288:nrow(reference)),]

#extracting response variables to be modelled 
resp <- reference [,c("Plot","X","Y","N.Dec","N.Con","N.Tot","N.TotStm","N.Trees","dbh_mean","H_mean","V","V_tot")] 


#selected columns from the lidar database
h2.00_hmax_selected <- h2.00_hmax [,c("FileTitle","Total.return.count.above.2.00","Elev.mean","Elev.stddev","Elev.CV","Elev.AAD",
                                      "Elev.L1","Elev.P10","Elev.P25","Elev.P75","Elev.P99","Canopy.relief.ratio")] 
XY <- resp[,c("X","Y")]
XY_sp <- SpatialPointsDataFrame(resp[,c("X","Y")], data=resp)

#extract the plot values from density metrics
hdens_plot <- extract(hdens, XY_sp, buffer=17.54, fun=mean, na.rm=T, df=T)

#put them together to form the predictor dataset
trainvals <- cbind(h2.00_hmax_selected, hdens_plot[,2],XY)

#change the name of the last column
names(trainvals)[names(trainvals) == 'hdens_plot[, 2]'] <- "canopycover"



## merge the predictors to form a single dataset for modelling 
data_merged <- cbind (resp, trainvals[, !(colnames(trainvals) %in% c("X","Y"))])


# Import some raster predictors for later wall-to-wall predictions
#####!!!!!#### Note: when predicting on wall-to-wall level, the same number and names of 
####predictors should be provided in grid form
### here we only import three of them, which is not enough for our wtw prediction
### please note this in your future modeling cases!!#######

grid_mean <- raster ("results\\metrics\\grid_mean_height.asc")
grid_sd <- raster ("results\\metrics\\grid_height_sd.asc")
grid_crr <- raster ("results\\metrics\\grid_crr.asc")

grid_stacked <- stack (grid_mean,grid_sd,grid_crr)


##################


#############################################
##  bootstrap selection of sample plots
#############################################

# create groups of same sizes, ordered according to their response values (input dataset has to be 
# ordered from lowest to highest biomass)
# to be modified for each input dataset/resposne

# sort the input table based on a specific response (here volume per ha)
data_merged_sorted <- data_merged[order(data_merged$V),] 

# create even row numbers
nr_rows <- nrow(data_merged_sorted )
nr_rows_1 <- ceiling(nr_rows / 3)
nr_rows_2 <- nr_rows - 2*nr_rows_1

#resume the above process
data_merged_sorted_g1 <- data_merged_sorted [1:nr_rows_1,]
data_merged_sorted_g2 <- data_merged_sorted [(nr_rows_1+1):(2*nr_rows_1),]
data_merged_sorted_g3 <- data_merged_sorted [(2*nr_rows_1+1):(nr_rows),]

# select bootstrapped subset of samples 

set.seed(1173)

# create empty lists in which subsets can be stored
input_cl1 <- list()

id_cl1 <- list()

id_cl1a <- list()

# start loop

N = 3
for (i in 1:N){
  
  # create random numbers with replacement to select samples from each group
  
  idx1=sample(1:nr_rows_1,(nr_rows_1)-1,replace=T)
  
  id_cl1[[i]] <- idx1
  
  # create random numbers with replacement to select samples from the last group which differs in 
  # length (due to fact that the full number of samples is not always smoothly dividable by 5)
  
  idx1a = sample(1:nr_rows_2, (nr_rows_2)-1,replace=T)
  
  id_cl1a[[i]] <- idx1a
  
  # select subsets of the three groups based on the random numbers
  
  g1_sub <- data_merged_sorted_g1[idx1,]
  g2_sub <- data_merged_sorted_g2[idx1,]
  g3_sub <- data_merged_sorted_g3[idx1a,]
  
  # combine the subsets of the groups
  
  samp_cl1 <- rbind(g1_sub, g2_sub, g3_sub)
  
  # store the combined groups to the list file
  
  input_cl1[[i]]<- samp_cl1
  
  
}


##################################
#
# result: one dataset: input_cl1 dataset forest inventory as input to the model building process
# the dataset has N repetitions of  bootstrapped subsets of different sizes of the original dataset
#
##################################


#############################################
## start regression modelling with caret
#############################################

#############################################
## obtain results 
#############################################

# create empty containers to store the results

SVMres <- matrix(0, ncol = 9, nrow = N)
GPres <- matrix(0, ncol = 9, nrow = N)
LMSTEPres <- matrix(0, ncol = 9, nrow = N)
RFres <- matrix(0, ncol = 9, nrow = N)
BRTres <- matrix(0, ncol = 9, nrow = N)

res_svm_cl1 <- list()
#res_knn_cl1 <- list()
res_gp_cl1 <- list()
res_lmStep_cl1 <- list()
res_rf_cl1 <- list()
res_BRT_cl1 <- list()


pred_svm_cl1 <- list()
#pred_knn_cl1 <- list()
pred_gp_cl1 <- list()
pred_lmStep_cl1 <- list()
pred_rf_cl1 <- list()
pred_BRT_cl1 <- list()

resp_cl1 <- list()

##Note: the full wall-to-wal prediction can be done for all modelling approaches, but here we only do it for RF

#fullpred_cl1_svm <- list()
#fullpred_cl1_gp <- list()
#fullpred_cl1_lmStep <- list()
fullpred_cl1_rf <- list()
#fullpred_cl1_BRT <- list ()


#change the wd
dir.create(paste(getwd(), "/results_maha/model_results/output_fullsamp", sep =""))

setwd("./results_maha\\model_results\\output_fullsamp")
## or use file.path ("a","b","c")


# run regressions for all 500 bootstrapped subsets of input_cl1

for (i2 in 1:N)
{
  
  # extract first bootstrapped datas-matrix, which contains predictors and response
  # list of class1 and transform into a dataframe
  
  
  input_rgr <- input_cl1[[i2]]
  input_rgr<-data.frame(input_rgr)
  
  input_rgr <- input_rgr[complete.cases(input_rgr), ]
  
  # define response (in this case second column of input dataset)
  response <- input_rgr[,11]
  
  # define predictors (in this case columns 3-18 of input dataset)
  predictors <- input_rgr[,14:ncol(input_rgr)]
  
  
  # define parameter tuning methods
  fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 5, returnResamp ="all")
  
  # tune different models and obtain fit results
  
  svmFit <- train(predictors, response, method = "svmRadial", trControl = fitControl)
  #svmFit$results
  svmPred <- predict(svmFit)
  svmRes <- svmPred - response
  #svmPredFull <- predict(fullTS, svmFit)
  
  gpFit <- train(predictors, response, method = "gaussprRadial", trControl = fitControl)
  #gpFit$results
  gpPred <- predict(gpFit)
  gpRes <- gpPred - response
  #gpPredFull <- predict(fullTS,gpFit)
  
  lmStepFit <- train(predictors, response, method = "lmStepAIC", trControl = fitControl,na.action= na.omit)
  #lmStepFit$results
  lmStepPred <- predict(lmStepFit)
  lmStepRes <- lmStepPred - response
  #lmStepPredFull <- predict(fullTS,lmStepFit)
  
  rfFit <- train(predictors, response, method = "rf", trControl = fitControl,na.action = na.omit)
  #rfFit$results
  rfPred <- predict(rfFit)
  rfRes <- rfPred - response
  ##please check the comment above concerning the wtw prediction
  #rfPredFull <- predict(grid_some,rfFit) 
  
  BRTFit <- train(predictors, response, method = "glmboost", trControl = fitControl)
  #brtFit$results
  BRTPred <- predict(BRTFit)
  BRTRes <-  BRTPred - response
  # BRTPredFull <- predict(fullTS,BRTFit)
  
  
  # extract RMSE and Rsquared values of all models (for each model the model with lowest
  # RMSE value is considered)
  
  #SVM
  #identify model with best setting in terms of lowest RMSE and store the index
  minRMSE <- min(svmFit$results$RMSE)
  svmi <- which(svmFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  #extract fit statistics for model with best settings with the help of the index
  #and store results into the already-made colmuns of the empty container created before the loop
  #columns 5-8 are added in order to allow ANOVA analysis
  #col5 = extrapolation method, col6 = number of samples, col7= input data, col8 = test site
  SVMres[i2,1] <- svmFit$results$RMSE[svmi]
  SVMres[i2,2] <- svmFit$results$Rsquared[svmi]
  SVMres[i2,3] <- svmFit$results$RMSESD[svmi]
  SVMres[i2,4] <- svmFit$results$RsquaredSD[svmi]
  SVMres[i2,5] <- "SVM"
  SVMres[i2,6] <- "CLASS2"
  SVMres[i2,7] <- "LIDAR"
  SVMres[i2,8] <- "KA"
  SVMres[i2,9] <- "N"
  

  #gp
  minRMSE <- min(gpFit$results$RMSE)
  gpi <- which(gpFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  GPres[i2,1] <- gpFit$results$RMSE[gpi]
  GPres[i2,2] <- gpFit$results$Rsquared[gpi]
  GPres[i2,3] <- gpFit$results$RMSESD[gpi]
  GPres[i2,4] <- gpFit$results$RsquaredSD[gpi]
  GPres[i2,5] <- "GP"
  GPres[i2,6] <- "CLASS2"
  GPres[i2,7] <- "LIDAR"
  GPres[i2,8] <- "KA"
  GPres[i2,9] <- "N"
  
  #lmStep
  minRMSE <- min(lmStepFit$results$RMSE)
  lmstepi <- which(lmStepFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  LMSTEPres[i2,1] <- lmStepFit$results$RMSE[lmstepi]
  LMSTEPres[i2,2] <- lmStepFit$results$Rsquared[lmstepi]
  LMSTEPres[i2,3] <- lmStepFit$results$RMSESD[lmstepi]
  LMSTEPres[i2,4] <- lmStepFit$results$RsquaredSD[lmstepi]
  LMSTEPres[i2,5] <- "LMSTEP"
  LMSTEPres[i2,6] <- "CLASS2"
  LMSTEPres[i2,7] <- "LIDAR"
  LMSTEPres[i2,8] <- "KA"
  LMSTEPres[i2,9] <- "N"
  
  #rf
  minRMSE <- min(rfFit$results$RMSE)
  rfi <- which(rfFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  RFres[i2,1] <- rfFit$results$RMSE[rfi]
  RFres[i2,2] <- rfFit$results$Rsquared[rfi]
  RFres[i2,3] <- rfFit$results$RMSESD[rfi]
  RFres[i2,4] <- rfFit$results$RsquaredSD[rfi]
  RFres[i2,5] <- "RF"
  RFres[i2,6] <- "CLASS2"
  RFres[i2,7] <- "LIDAR"
  RFres[i2,8] <- "KA"
  RFres[i2,9] <- "N"
  
  #   brt
  minRMSE <- min(BRTFit$results$RMSE)
  brti <- which(BRTFit$results$RMSE==minRMSE,arr.ind=TRUE)
  #   
  BRTres[i2,1] <- BRTFit$results$RMSE[brti]
  BRTres[i2,2] <- BRTFit$results$Rsquared[brti]
  BRTres[i2,3] <- BRTFit$results$RMSESD[brti]
  BRTres[i2,4] <- BRTFit$results$RsquaredSD[brti]
  BRTres[i2,5] <- "BRT"
  BRTres[i2,6] <- "CLASS1"
  BRTres[i2,7] <- "LIDAR"
  BRTres[i2,8] <- "KA"
  BRTres[i2,9] <- "N"
  
  
  res_svm_cl1[[i2]] <- svmRes
  res_gp_cl1[[i2]] <- gpRes
  res_lmStep_cl1[[i2]] <- lmStepRes
  res_rf_cl1[[i2]] <- rfRes
  res_BRT_cl1[[i2]] <- BRTRes
  
  pred_svm_cl1[[i2]] <- svmPred
  pred_gp_cl1[[i2]] <- gpPred
  pred_lmStep_cl1[[i2]] <- lmStepPred
  pred_rf_cl1[[i2]] <- rfPred
  pred_BRT_cl1[[i2]] <- BRTPred
  
  resp_cl1[[i2]] <- response
  
  # fullpred_cl1_svm[[i2]] <- svmPredFull
  # fullpred_cl1_gp[[i2]] <- gpPredFull
  # fullpred_cl1_lmStep[[i2]] <- lmStepPredFull
 # fullpred_cl1_rf[[i2]] <- rfPredFull
  # fullpred_cl1_BRT[[i2]] <- BRTPredFull
  
}


##save the wtw predictions####

#save(fullpred_cl1_svm, file = "fullpred_cl1_svm.RData")
#save(fullpred_cl1_gp, file = "fullpred_cl1_gp.RData")
#save(fullpred_cl1_lmStep, file = "fullpred_cl1_lmStep.RData")
save(fullpred_cl1_rf, file = "fullpred_cl1_rf.RData")
#save(fullpred_cl1_BRT, file = "fullpred_cl1_BRT.RData")

### print results to textfiles


write.table(SVMres, file="LIDAR_cl1_SVMres.txt", sep="\t")
write.table(GPres, file="LIDAR_cl1_GPres.txt", sep="\t")
write.table(LMSTEPres, file="LIDAR_cl1_LMSTEPres.txt", sep="\t")
write.table(RFres, file="LIDAR_cl1_RFres.txt", sep="\t")
write.table(BRTres, file="LIDAR_Cl1_BRTres.txt", sep="\t")



### print residual plots

# print residuals of svm

tiff(filename = "Residuals_cl1_SVM.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)


for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_svm_cl1[[i2]], ylim = range (res_svm_cl1), xlim=range (resp_cl1), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_svm_cl1[[N]], ylim = range (res_svm_cl1), xlim=range (resp_cl1), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot SVM" )
abline(h=0)

dev.off()  


# print residuals of gp
tiff(filename = "Residuals_cl1_GP.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_gp_cl1[[i2]], ylim=range (res_gp_cl1), xlim=range (resp_cl1), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_gp_cl1[[N]], ylim = range (res_gp_cl1), xlim=range (resp_cl1), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot GP" )
abline(h=0)

dev.off()  

# print residuals of lmstep
tiff(filename = "Residuals_cl1_lmStep.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_lmStep_cl1[[i2]], ylim=range (res_lmStep_cl1), xlim=range (resp_cl1), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_lmStep_cl1[[N]],  ylim=range (res_lmStep_cl1), xlim=range (resp_cl1), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot lmStep" )
abline(h=0)

dev.off() 

# print residuals of rf


tiff(filename = "Residuals_cl1_RF.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_rf_cl1[[i2]],  ylim=range (res_rf_cl1), xlim=range (resp_cl1), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_rf_cl1[[N]], ylim=range (res_rf_cl1), xlim=range (resp_cl1), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot RF" )
abline(h=0)

dev.off()  



# print residuals of BRT
tiff(filename = "Residuals_cl1_BRT.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_BRT_cl1[[i2]], ylim = range (res_BRT_cl1), xlim=range (resp_cl1), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_BRT_cl1[[N]], ylim = range (res_BRT_cl1), xlim=range (resp_cl1), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot BRT" )
abline(h=0)

dev.off()  

### save residuals, predicted and response values to .RData files
save(resp_cl1, file = "resp_cl1_lidar.RData")

save(res_svm_cl1, file = "res_svm_cl1_lidar.RData")
save(res_gp_cl1, file = "res_gp_cl1_lidar.RData")
save(res_lmStep_cl1, file = "res_lmStep_cl1_lidar.RData")
save(res_rf_cl1, file = "res_rf_cl1_lidar.RData")
save(res_BRT_cl1, file = "res_BRT_cl1_lidar.RData")

##save the predictions to file####
save(pred_svm_cl1, file = "pred_svm_cl1_lidar.RData")
save(pred_gp_cl1, file = "pred_gp_cl1_lidar.RData")
save(pred_lmStep_cl1, file = "pred_lmStep_cl1_lidar.RData")
save(pred_rf_cl1, file = "pred_rf_cl1_lidar.RData")
save(pred_BRT_cl1, file = "pred_BRT_cl1_lidar.RData")


