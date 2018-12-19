################
####ggsoccer####
################

#--------------------------https://github.com/Torvaney/ggsoccer-------------------------------#

library(ggplot2)
devtools::install_github("torvaney/ggsoccer")
library(ggsoccer)

#-----------------------------------------------------------------
#plot a set of passes onto a soccer pitch
pass_data <- data.frame(x = c(24, 18, 64, 78, 53),
                        y = c(43, 55, 88, 18, 44),
                        x2 = c(34, 44, 81, 85, 64),
                        y2 = c(40, 62, 89, 44, 28))

ggplot(pass_data) +
  annotate_pitch() +
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  theme_pitch() +
  direction_label() +
  xlim(-1, 101) +
  ylim(-5, 101) +
  ggtitle("Simple passmap", 
          "ggsoccer example")

#------------------------------------------------------------------
#simple shoot map
shots <- data.frame(x = c(90, 85, 82, 78, 83, 74),
                    y = c(43, 40, 52, 56, 44, 71))

ggplot(shots) +
  annotate_pitch(colour = "gray70",
                 fill = "gray90") +
  geom_point(aes(x = x, y = y),
             fill = "white", 
             size = 4, 
             pch = 21) +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  ggtitle("Simple shotmap",
          "ggsoccer example")

#------------------------------------------------------------------
#Rescale shots to use StatsBomb-style coordinates
shots_rescaled <- data.frame(x = shots$x * 1.20,
                             y = shots$y * 0.80)

ggplot(shots_rescaled) +
  annotate_pitch(x_scale = 1.2,
                 y_scale = 0.8,
                 colour = "gray70",
                 fill = "gray90") +
  geom_point(aes(x = x, y = y),
             fill = "white", 
             size = 4, 
             pch = 21) +
  theme_pitch() +
  coord_flip(xlim = c(59, 121),
             ylim = c(-1, 81)) +
  ggtitle("Simple shotmap",
          "ggsoccer example (120x80 co-ordinates)")

