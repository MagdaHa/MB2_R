###########################
####Homework lecture 05####
###########################
library(ggplot2)

#set Working Directory
setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\MB2_R\\lecture_05")

#loading data
Census.Data <- read.csv("practical_data.csv")
names(Census.Data)

#scatter plot with ggplot2
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point()
#scatter plot with good visualization
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point(aes(colour = White_British, size = Low_Occupancy))+
  labs(title="Relationship between age and unemployment in the UK")