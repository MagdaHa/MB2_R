#############
##Functions##
#############

##if-else
#example 1
a <- 5
if(a<0)                             # if a is langer than 0 do....
{
  print("it is apositive number")
}

#example 2
b <- 6
if(a!=6)                           # if b is unequal 6 do....
{
  print("number is not equal 6")
} else {
  print ("nuber is equal 6")
}

#-------------------------------------------------------------------------------------------
##multiple if-else
set.seed(100)
abc <- sample(letters[1:5], 1000, replace=T)
df <- data.frame(v1=abc, v2="blank", stringsAsFactors = F)
head(df)

system.time({
  df$v2 <- ifelse(df$v1=="a", "apple",
                  ifelse(df$v1=="b", "ball",
                  ifelse(df$v1=="c", "cat",
                  ifelse(df$v1=="d", "dog", "elephant"))))
})
head(df)

#or------------------------------------------------
##alternative to nested if-else statements
for (i in 1:nrow(df)){
  val <- df$v1[i]
  df$v2[i] <- switch(val,
                     "a"="apple",
                     "b"="ball",
                     "c"="cat",
                     "d"="dog",
                     "e"="elephant")
}
head(df)

#--------------------------------------------------------------------------------------------
##while statement
j <-0
while (j<1)
{
  j<-j+0.1; print(j)
}

#------------------------------------------------------------------------------------------
##creating own functions
myfunction <- function(x,y){
  z <- x+y
  return(z)
}
myfunction(3,4)

#example ndvi
ndvi <- function(nir, red){
 return((nir-red)/(nir+red))
}
ndvi(5,7)

#------------------------------------------------------------------------------------------
##function with sentence as output

greet <- function(name) {
  paste0("How do you do, ", name, "?")
}
greet("Magda")
