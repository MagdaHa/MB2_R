#####################
####sapply lapply####
#####################

#sapply
sapply(1:3, function(x)x^2) #all values will run trough this function

#or lapply
lapply(1:3, function(x)x^2) #returns a list

#--------------------------------------------------------------------------
##create a list with various entries
x<- list(a=1:10, beta=exp(-3:3), logic=c(TRUE, FALSE, FALSE, TRUE))

#return the mean of each list entry
lapply(x, mean)
