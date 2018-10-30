install.packages("fun")
library(fun)
if(.Platform$OS.type == "windows")x11( ) else x11 (type = " Xlib")
mine_sweeper ( )
# orgomoku( )

##sudoku##
install.packages("sudoku")
library(sudoku)
if(.Platform$OS.type == "windows")x11( ) else x11 (type = " Xlib")
playSudoku()

 #mario??##
install.packages("mario")
library(mario)
if(.Platform$OS.type == "windows")x11( ) else x11 (type = " Xlib")
MarioKario()

##########################################
#colourfindr
install.packages("colorfindr")
install.packages("devtools")
devtools::install_github("zumbov2/colorfindr")

# Load packages
pacman::p_load(colorfindr, dplyr)

# Plot
get_colors(
  img = "https://upload.wikimedia.org/wikipedia/commons/a/af/Flag_of_South_Africa.svg",
  min_share = 0.05
) %>%
  plot_colors(sort = "size")
# Load packages
pacman::p_load(colorfindr, dplyr)

# Load packages
pacman::p_load(colorfindr, dplyr)

# Images
img <- c("https://upload.wikimedia.org/wikipedia/commons/b/b5/Wappen_Aargau_matt.svg")

# Plot
for (i in 1:length(img)) get_colors(img[i], top_n = 4)
plot_colors(sort = "size")

# Plot (5000 randomly selected pixels)
get_colors("https://upload.wikimedia.org/wikipedia/commons/f/f4/The_Scream.jpg") 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")