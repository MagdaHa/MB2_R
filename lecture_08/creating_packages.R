#############################
####creating own packages####
#############################


#install and activate development package
install.packages('devtools', dependencies = TRUE)
library('devtools')
library('roxygen2')

#check if you have a development environment installed
has_devel()

setwd("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann")
create("magda")
getwd()


#----------------------------------------------------
#after writing some tools, the packages has to be installed --> every each new added script

#setwd("path to the package directory")("C:\\02_Studium\\02_Master\\01_Semester 1\\MB2_Introduction to Programming and Geostatistics_Wegmann\\MB2_R\\magda")
load_all()
document()
install()

#----------------------------------------------------
#checking all @example options which were set
run_examples()
check()

#-----------------------------------------------------
# building the package and saving the compressed package in wd
build()             #bundled .tar.gz  
build(binary=TRUE)  #binary .tar.bz or .zip
build_win()         #binary .zip

#-------------------------------------------------------
#stores installed packages fust for your development
dev_mode(on=TRUE)
install("magda")
library(magda)

dev_mode(on=FALSE) # to uninstall the dev mode
library(magda)

#--------------------------------------------------------
#using other user defiend packages
install_github("wang4918/mypackage")
library(mypackage)
mypackage::sumofcubes(2,3)
