##############################
####reshaping data outputs####
##############################

install.packages("reshape2")
library(reshape2)

field_data <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv", header=TRUE)
head(field_data)      
summary(field_data)
names(field_data)

melt(field_data, id.vars =c("L7.ndvi", "LCname"))
