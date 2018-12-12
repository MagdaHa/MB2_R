###################
####pipes - %>%####
###################

install.packages("babynames")
library(babynames)

babynames <- babynames::babynames                    #select babynames from library babynames
temp1 <- filter(babynames, sex=="M", name=="Taylor") #filter babyname Taylor
temp2 <- select(temp1, n)
temp3 <- sum(temp2)

#--------------------------------------
#or with pipes %>%

babynames %>%filter(sex=="M", name=="Taylor") %>%
  select(n)
sum
