################
##just for fun##
################

install.packages("fortunes")
library (fortunes)
fortune()
fortune("memory")

#random animals1
#install.packages("cowsay")
library(cowsay)
someone_say_hello <- function(){
  animal <- sample(names(animals), 1)
  say(paste("Hello, I am ", animal, ".", collapse = ""), by = animal)
}
someone_say_hello()

#random animals2
someone_say_my_fortune <- function() {
  animal <- sample(names(animals), 1)
  say(paste(fortune(), collapse = "\n"), by = animal)
}
someone_say_my_fortune()