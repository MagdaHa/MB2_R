#############
####RCurl####
#############

library(RCurl)
library(bitops)

# get the data
x<- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbXxJqjfY-voU-9UWgWsLW09z4dzWsv9c549qxvVYxYkwbZ9RhGE4wnEY89j4jzR_dZNeiWECW9LyW/pub?gid=0&single=true&output=csv")))

#inspect the data
x; summary(x)

#------------------------------------------------
library(reshape)
x2 <- melt(data=x)
library(ggplot2)
ggplot(x2, aes(x=variable, y=value))+geom_boxplot()

#-------------------------------------------------
#plot points plus boxplot and add jitter, adding variable"cumsum"
x.cs <- data.frame(variable=names(x), cs=t(cumsum(x)[nrow(x),]))
names(x.cs) <- c("variable", "cumsum")
x2 <- melt(data=x)

x3 <- merge(x.cs, x2, by.x="variable", all=T)

ggplot(x3, aes(x=variable, y=value, color=cumsum))+geom_point()

ggplot(x3, aes(x=variable, y=value, color=cumsum))+geom_boxplot(alpha=.5)+geom_point(alpha=.7, size=1.5, position=position_jitter(width=.25, height=.5))

#----------------------------------------------------
install.packages("gender")
library(gender)
library(genderdata)

x.g <- gender(names(x))

#-----------------------------------------------------
colnames(x.g)[1] <- "variable"
x4 <- merge(x3, x.g, by.x="variable", all=T)
a <- ggplot(x4, aes(x=variable, y=value, color=cumsum))+geom_boxplot()+facet_wrap(~gender)
a

#---------------------------------------------------------
#adjust the graph
a + coord_flip()
a+theme(axis.text.x=element_text(angle = 45, vjust=1, hjust=1))

#-------------------------------------------------------
#removing male names from female prolt and vice versa
a <- ggplot(x4, aes(x=variable, y=value, color=cumsum))+geom_boxplot()+facet_wrap(~gender, scales="free_x")
a+theme(axis.text.x=element_text(angle = 45, vjust=1, hjust=1))
