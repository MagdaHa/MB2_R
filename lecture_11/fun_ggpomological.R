##################################
####fun package: ggpomological####
##################################
#----------------------https://github.com/gadenbuie/ggpomological------------------------#

# If you don't have devtools installed
install.packages("devtools")
devtools::install_github("gadenbuie/ggpomological")
install.packages("colorspace")
library(colorspace)
install.packages("rlang")
library(rlang)
# To include the vignette
devtools::install_github("gadenbuie/ggpomological", build_vignettes=TRUE)
library(ggpomological)
library(ggplot2)

scales::show_col(ggpomological:::pomological_palette)
scales::show_col(unlist(ggpomological:::pomological_base))

#----------------------------------------------------------------------------------------------
# Base plot
basic_iris_plot <- ggplot(iris) +
  aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
  geom_point(size = 2)
# Just your standard Iris plot
basic_iris_plot 

#---------------------------------------------------------------------------------------------
# With pomological colors
basic_iris_plot <- basic_iris_plot + scale_color_pomological()
basic_iris_plot

# With pomological theme
basic_iris_plot + theme_pomological()

# With transparent background
basic_iris_plot + theme_pomological_plain() 

# Or with "fancy" pomological settings
pomological_iris <- basic_iris_plot + theme_pomological_fancy()

# Painted!
paint_pomological(pomological_iris, res = 110)

#-------------------------------------------------------------------------------------------
# stacked bar chart
stacked_bar_plot <- ggplot(diamonds) +
  aes(price, fill = cut) +
  geom_histogram(binwidth = 850) + 
  xlab('Price (USD)') + 
  ylab('Count') + 
  ggtitle("ggpomological") +
  scale_x_continuous(label = scales::dollar_format()) +
  scale_fill_pomological()

stacked_bar_plot + theme_pomological("Homemade Apple", 16)

paint_pomological(
  stacked_bar_plot + theme_pomological_fancy(),
  res = 110
)

#-------------------------------------------------------------------------------------------
install.packages("mutate")
library(mutate)
density_plot <- mtcars
  mutate(cyl = factor(cyl))
  ggplot() +
  aes(mpg, fill = cyl, color = cyl)+
  geom_density(alpha = 0.75) + 
  labs(fill = 'Cylinders', colour = 'Cylinders', x = 'MPG', y = 'Density') +
  scale_color_pomological() +
  scale_fill_pomological()

density_plot + theme_pomological("Homemade Apple", 16)

#----------------------------------------------------------------------------------------
paint_pomological(
  density_plot + theme_pomological_fancy(),
  res = 110
)

#--------------------------------------------------------------------------------------
#points and lines
big_volume_cities <- txhousing
  group_by(city) 
  summarize(mean_volume = mean(volume, na.rm = TRUE))
  arrange(-mean_volume)
  top_n(length(ggpomological:::pomological_palette))
  pull(city)
# Selecting by mean_volume

full_bar_stack_plot <- txhousing %>% 
  filter(city %in% big_volume_cities) %>% 
  group_by(city, year) %>% 
  summarize(mean_volume = mean(volume, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(city = factor(city, big_volume_cities)) %>% 
  ggplot() +
  aes(year, mean_volume, fill = city, group = city) +
  geom_col(position = 'fill', width = 0.9) +
  labs(x = 'City', y = 'Mean Volume', color = 'City') +
  theme(panel.grid.minor.x = element_blank()) +
  scale_fill_pomological()

full_bar_stack_plot + theme_pomological("Homemade Apple", 16)

paint_pomological(
  full_bar_stack_plot + theme_pomological_fancy(),
  res = 110
)
#-------------------------------------------------------------------------------------------
ridges_pomological <- ggplot(diamonds) + 
  aes(x = carat, y = clarity, color = clarity, fill = clarity) + 
  ggridges::geom_density_ridges(alpha = 0.75) + 
  theme_pomological(
    base_family = 'gWriting',
    base_size = 20,
    base_theme = ggridges::theme_ridges()
  ) + 
  scale_fill_pomological() + 
  scale_color_pomological()

paint_pomological(ridges_pomological, res = 110)
#> Picking joint bandwidth of 0.057