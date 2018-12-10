#############################
####gglplot2: chord diagram####
#############################

#---------------https://guyabel.com/post/animated-directional-chord-diagrams/-----------#

install.packages("migest")
library(migest)
library(tidyverse)

#Time series of bilateral migration flow estimates
d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"))
d0

#Some regional meta data for chord diagram plots
d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))
d1

#The next step is to tween the data by migration corridor
library(tweenr)

d2 <- d0 %>%
  mutate(corridor = paste(orig_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year0", group = "corridor", ease = "ease", nframes = 100) %>%
  tbl_df()
d2
d2 <- d2 %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow/1e06)
d2


#-----------------------
# create a directory to store the individual plots
dir.create("./plot-gif/")

#install.packages("circlize")
library(circlize)
for(f in unique(d2$.frame)){
  # open a PNG plotting device
  png(file = paste0("./plot-gif/globalchord", f, ".png"), height = 7, width = 7, 
      units = "in", res = 500)
  
  # intialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex=1)
  circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
             gap.degree = 4, points.overflow.warning = FALSE)
  
  # plot the chord diagram
  chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region,
               grid.col = d1$col1, annotationTrack = "grid",
               transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
               direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
               diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE)
  
  # add labels and axis
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% filter(region == sector.index) %>% pull(reg2)
    
    circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 3, 4),
                labels = reg1, facing = "bending", cex = 1.1)
    circos.text(x = mean(xlim), y = 2.75, labels = reg2, facing = "bending", cex = 1.1)
    circos.axis(h = "top", labels.cex = 0.8
                labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
  })
  
  # close plotting device
  dev.off()
}
