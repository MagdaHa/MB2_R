#######################
####sun calculation####
#######################
####https://github.com/datastorm-open/suncalc####

install.packages("suncalc")
install.packages("V8")

library(suncalc)
library(V8)

#-----------------------------------------------------------------------------------------
getSunlightTimes (date = Sys.Date(), lat = 49.782332, lon = 9.970187, tz = "CET")
