####https://github.com/zumbov2/colorfindr####

install.packages("colourfindr")
install.packages("dplyr")
install.packages("pacman")

library(colorfindr, dplyr, pacman)
library(pacman)
# Load packages
pacman::p_load(colorfindr, dplyr)


# Plot of the southern african flag
get_colors(
  img = "https://upload.wikimedia.org/wikipedia/commons/a/af/Flag_of_South_Africa.svg",
  min_share = 0.05
) %>%
  plot_colors(sort = "size")


#3D scatterplot of "the scream" 
#Plot (5000 randomly selected pixels)
get_colors("https://upload.wikimedia.org/wikipedia/commons/f/f4/The_Scream.jpg") %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")