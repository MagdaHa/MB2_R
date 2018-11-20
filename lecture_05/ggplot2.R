###############
####ggplot2####
###############

help.search("geom_", package = "ggplot2")

library(ggplot2)
x11()                                                             #start of extern graphic window
x <- data.frame(x=1, y=1, label="ggplot2 introduction \n@ EAGLE") #\n <- in new line
ggplot(data=x, aes(x=x, y=y))+geom_text(aes(label=label), size=15)

#-------------------------------------------------------------------
#dataframe
x1 <- rnorm(1000,0,1)
x2 <- rnorm(1000,5,10)
x3 <- rep(c("catA", "catB", "catC", "catC", "catC"), 200)[1:1000]
x4 <- factor(rep(c("yes", "no"), 500))

df <- data.frame(a=x1, b=x2, c=x3, d=x4)
ggplot(df, aes(a, b)) +geom_point()                               #geom_point <- adding geometry
ggplot(df, aes(a, b, color=c)) +geom_point()                      #add a color <- color of c 
ggplot(df, aes(a, b, color=c)) +geom_point(alpha=.5)              #alpha <- translucency of 0.5
ggplot(df, aes(a, b, color=d)) +geom_point(alpha=.5) +            #title
  labs(title="first plot", x = "x axis \n and a new line")

#-------------------------------------------------------------------
#histogram
ggplot(df, aes(a))+
  geom_histogram(color="orange")                                  #orange outline colour of bars

#-------------------------------------------------------------------
#density graph
ggplot(df, aes(c, fill=c))+                                       #colors with variable c
  geom_density(alpha=.5)

#------------------------------------------------------------------
#combining graphs
ggplot(df)+ 
  geom_histogram(aes(a,..density..), fill="blue", color ="pink")+
  geom_density(aes(a, ..density..), color = "yellow")+
  geom_rug(aes(a))

#------------------------------------------------------------------
#counting statistics
ggplot(df, aes(c, color=c))+
  geom_point(stat = "count", size=6, alpha=.5)+
  coord_flip()+                                                   #flip axes, alpha=translucency of dots
  theme_dark()+                                                   #background color dark rey
  scale_color_grey()                                              #dots in grey
  
#-----------------------------------------------------------------
#flipped bar plot
ggplot(df) + geom_bar(aes(c)) + coord_flip()

#------------------------------------------------------------------
#bar plot grouped by category
ggplot(df, aes(d, fill=c))+
  geom_bar(position = "dodge")+
  scale_fill_grey()                                               #bars in grey values

#-------------------------------------------------------------------
#boxplot with categorial variables
ggplot(df, aes(d, a)) +geom_boxplot()
ggplot(df, aes(d, a)) +geom_boxplot()+
  geom_jitter()                                                   #adding jitter
ggplot(df, aes(d, a)) +geom_boxplot(alpha=.5)+
  geom_jitter(alpha=.5, width=0.3, color="blue")+                 #changing colour of jitter, adding translucency
ggplot(df, aes(d, a))+
  geom_boxplot(aes(group=cut_width(a, 0.5)), outlier.alpha = 0.1)+
  geom_jitter(width=0.3, color="blue")                            #cutting contious values for using in in a boxplot
  
#-------------------------------------------------------------------
#facet plot (slit by predefined category)
ggplot(df, aes(a,b,color=d))+                                     #a and b as axes, d as color
  geom_point(alpha=.5)+facet_grid(c~.)

#------------------------------------------------------------------
#2D density plot
ggplot(df, aes(a,b))+
  geom_point(size=1)+
  geom_density2d()

ggplot(df, aes(a,b))+
  geom_hex(bins=30)                                                 #hexagons as density indicator visualized by color

#-----------------------------------------------------------------
#adding a regression line
ggplot(df, aes(a,b))+
  geom_point()+ geom_smooth(method = lm)

#------------------------------------------------------------------
#storing ggplot in variables
a <- ggplot()+ geom_point(data=df, aes(a, b, colour=c))     
a+theme_bw()       
