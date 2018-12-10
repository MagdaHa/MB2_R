####################
####colorblindr#####
####################
#---------------------------https://github.com/clauswilke/colorblindr---------------------#
devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos="http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")

#------------------------------------------------------------------------------------------
library(ggplot2)
fig <- ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
fig

#-----------------------------------------------------------------------------------------
library(colorblindr)
cvd_grid(fig)
view_cvd(p) 

#-----------------------------------------------------------------------------------------
fig2 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7) + scale_fill_OkabeIto()
fig2
cvd_grid(fig2)