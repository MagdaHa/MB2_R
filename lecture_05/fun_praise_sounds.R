###########################
#####praise and sounds#####
###########################


install.packages("devtools")
install.packages("Rtools")
library(devtools)

#-----------------------------------------------------------------------------
#praise
devtools::install_github("gaborcsardi/praise")
library(praise)
praise()

#------------------------------------------------------------------------------
#sounds
if(!require(devtools)) {install.packages(devtools)}
devtools::install_github("brooke-watson/BRRR")
library(BRRR)

skrrrahh("drummaboy")
skrrrahh("snoop")
skrrrahh(13)  #choose number of sound 23, 50


#------------------------------------------------------------------------------
#beep (https://www.r-project.org/nosvn/pandoc/beepr.html)
install.packages("beepr")
library(beepr)
beep(8) #sounds from 1 to 10
        #1 ping; 2 coin; 3 fanfare; 4 complete; 5 treasure; 6 ready; 7 shotgut; 8 mario; 9 wilhelm; 10 facebook

