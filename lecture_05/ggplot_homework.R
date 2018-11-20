###########################
####Homework lecture 05####
###########################


#set Working Directory
setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\MB2_R\\lecture_05")

#------------------------------------------------------------------------------------------------------------------------------------
#loading data
Census.Data <- read.csv("practical_data.csv")
names(Census.Data)

#------------------------------------------------------------------------------------------------------------------------------------
#loading library ggplot2
library(ggplot2)

#------------------------------------------------------------------------------------------------------------------------------------
#scatter plot with ggplot2
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point()

#-----------------------------------------------------------
#scatter plot with good visualization
p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point(aes(colour = White_British, size = Low_Occupancy))+
  labs(title="Correlation between age and occupancy \n in relation to qualification and unemployment rate in the UK")+   #title#displayed variables, shown in size and colour
  theme_dark()+                                  #background colour
  theme(legend.position = "right")               #legend position
  
