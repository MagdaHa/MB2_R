#############################################################################
####examples of working with airborne point clouds inside forestr library####
#########Ecosystem and Canopy Structural Complexity Metrics from LiDAR#######
#############################################################################

# This package Provides a toolkit for calculating forest and canopy structural complexity metrics
#from terrestrial LiDAR

#load the package
install.packages("forestr")
library (forestr)
#######################################################################################################

# examples for forestr package
#######################################################################################################
# a function to calculate the effective number of layers in a canopy

# Calculates the effective number of layers
calc_enl(pcl_vai)
#######################################################################################################
#######################################################################################################
# a function to produce clumping index based on gap fraction through the canopy

calc_gap_fraction(pcl_vai)

#######################################################################################################
#another function to calculate canopy structural complexity metrics from PCL data and prints them 

# Calculates metrics of canopy structural complexity.
calc_rugosity(pcl_summary, pcl_vai, filename = "")
#######################################################################################################
# a function to use the summary matrix created by the function make_summary_matrix to calculate
#canopy rumple (the relationship between outer canopy surface and the ground area)

calc_rumple(pcl_summary)

#######################################################################################################
# a function that derives mean leaf height from x, z vai from TLS data
# vai stands for vegetation area index 

# with data frame
process_pcl(osbs, marker.spacing = 10, user_height = 1.05, max.vai = 8)

#######################################################################################################
# s function to calculate vegetation area index (VAI) from a normalized matrix of LiDAR data.

pcl_vai <- calc_vai(pcl_norm, max.vai = 8)

#######################################################################################################
# a funciton to create first-order canopy structural metrics that do not require normalization

csc.metrics <- csc_metrics(pcl_adjusted, filename = "UVA", transect.length = 10)
#######################################################################################################
# A funciton to make PCL matrix for higher level complexity measures
# This function produces a matrix of, x, z values in coordinate space with the number and type of each LiDAR return in each x, z bin combination

pcl_matrix <- make_matrix(pcl_split)

#######################################################################################################
# A function to produce acreates a summary matrix of data through data wrangling the VAI data frame

pcl_summary <- make_summary_matrix(pcl_split, pcl_vai)

#######################################################################################################
# A funciton to normalize PCL data based on light saturation and attenuation

pcl_norm <- normalize_pcl(pcl_matrix)
#######################################################################################################
# A function to produce a PCL diagnostic plot

pcl_diagnostic_plot(osbs)

#######################################################################################################
# A function that produces a LiDAR hit grid plot
# Calculates metrics of canopy structural complexity.

plot_hit_grid(pcl_vai, filename = "UVA LiDAR data", transect.length = 40,
              max.ht = 30, max.vai = 8)
#######################################################################################################
# A function that graphs Plant Area Volume Density Profiles

# Calculates metrics of canopy structural complexity.
plot_pavd(pcl_vai, hist = FALSE, output.file = FALSE)
plot_pavd(pcl_vai, hist = TRUE, output.file = FALSE)
#######################################################################################################
# A function to import and process a single PCL transect 
# Note: the function process_multi_pcl () is also available to process multiple transects

# Link to stored, raw PCL data in .csv form
uva.pcl <- system.file("extdata", "UVAX_A4_01W.csv", package = "forestr")
# Run process complete PCL transect, store output to disk
process_pcl(uva.pcl, marker.spacing = 10, user_height = 1.05,
            max.vai = 8, pavd = FALSE, hist = FALSE)
#######################################################################################################
# A function to import PCL or portable canopy LiDAR files into the workspace and formats them

# Link to raw PCL data, in .csv form.
uva_pcl <- system.file("extdata", "UVAX_A4_01W.csv", package = "forestr")
# Import PCL data to the workspace
pcl_data <-read_pcl(uva_pcl)
#######################################################################################################


########################################END OF CODE####################################################
#######################################################################################################
















