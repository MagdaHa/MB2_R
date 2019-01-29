##################
####statistics####
##################

plot(iris[,1:4])


x<- runif(100)
y <- runif(100)
plot(x,y)
cor(x, y)

#-------------------------------------
install.packages("TeachingDemos")
library(TeachingDemos)
install.packages("tkrplot")
library(tkrplot)

if(interactive()){
  run.cor2.examp()
}

if(interactive()){
  put.points.demo()
  
  x<- rnorm(25,5,1)
  y<- x+rnorm(25)
  put.points.demo(x, y)
}

#--------------------------------------------
source("http://janhove.github.io/RCode/plot_r.R")
plot_r(r=0.5, n=50)
plot_r(r=0.8, n=50)

#----------------------------------------------
data(stork)
plot(stork)
summary(stork)

cor(stork$No.storks, stork$No.babies)
cor.test(stork$No.storks, stork$No.babies)




