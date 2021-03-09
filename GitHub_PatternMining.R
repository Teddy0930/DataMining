rm(list=ls())
# plyr
#
if (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}
# ??plyr
# dplyr
#
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(arules)) {
  install.packages("arules")
  library(arules)
}
# ??dplyr
###
# Input file specification
# The first two columns must be called namelist and eventlist, respectively.
# The two columns are then regarded as transaction id and items, respectively. 
###
# Choose file: "2017-07-9.csv"
#
in.file <- file.choose()
wd <- paste0(gsub("^(.*)\\\\.*$",'\\1',in.file),"\\") # working directory
git.frame <- read.csv(in.file)[,-1] # remove the 1st column
View(git.frame)
head(git.frame)
#
if (!require(forecast)) {
  install.packages("forecast")
  library(forecast)
}


rm(date1)
#cut one day into 4group
library("lubridate")
date1 <- git.frame[1:151487,]
date2 <-git.frame[151488:324110,]
date3 <- git.frame[32411:538699,]
date4 <- git.frame[538700:735851,]
?
  # 0:00~6:00
  trans.list1 <- date1 %>%
  group_by(namelist,eventlist) %>%
  arrange(namelist) %>%
  tally() %>%
  select(-one_of("n")) %>%
  rename(tid=namelist,item=eventlist) %>%
  as.data.frame()
head(trans.list1)
str(trans.list1)
#
one <- ddply(trans.list1,"tid",function(df) paste(df$item,collapse=","))
names(one)[2] <- "items"
head(one)
str(one)
#
itemsets1 <- one[,2,drop=F]
head(itemsets)
str(itemsets)
#
out.file <- tempfile() # temporary file name
write.csv(itemsets1,out.file,row.names=F,quote=F)
git.trans1 <- read.transactions(out.file,format="basket",sep=",",skip=1)
file.remove(out.file) # remove the temporary file
class(git.trans1)
head(git.trans1)
inspect(git.trans1[3]) # the 3rd transaction
summary(git.trans1)

#Frequent
freq1 <- apriori(git.trans1, 
                 parameter=list(supp=0.01,target="rules"))
summary(freq1)
freq1=sort(freq1,decreasing=T,by="lift")
inspect(freq1)
#maximal
Max1 <- is.maximal(freq1)
Max1
#
rules <- apriori(git.trans1, 
                 parameter=list(supp=0.01,target="rules"))
summary(rules)
rules=sort(rules,decreasing=T,by="lift")
inspect(rules)


#6:00~12:00
trans.list1 <- date2 %>%
  group_by(namelist,eventlist) %>%
  arrange(namelist) %>%
  tally() %>%
  select(-one_of("n")) %>%
  rename(tid=namelist,item=eventlist) %>%
  as.data.frame()
#
#
second <- ddply(trans.list1,"tid",function(df) paste(df$item,collapse=","))
names(second)[2] <- "items"
head(second)
str(second)
#
itemsets2 <- second[,2,drop=F]
head(itemsets2)
str(itemsets2)
#
out.file <- tempfile() # temporary file name
write.csv(itemsets2,out.file,row.names=F,quote=F)
git.trans2 <- read.transactions(out.file,format="basket",sep=",",skip=1)
file.remove(out.file) # remove the temporary file
class(git.trans2)
head(git.trans2)
inspect(git.trans2[3]) # the 3rd transaction
summary(git.trans2)
#
#Frequent
freq2 <- apriori(git.trans2, 
                 parameter=list(supp=0.01,target="rules"))
summary(freq2)
freq2=sort(freq2,decreasing=T,by="lift")
inspect(freq2)
#maximal
Max2 <- is.maximal(freq2)
Max2
#
rules <- apriori(git.trans2, 
                 parameter=list(supp=0.001,conf=0.5,target="rules"))
summary(rules)
inspect(sort(rules,by="lift"))
#12:00~18:00
trans.list1 <- date3 %>%
  group_by(namelist,eventlist) %>%
  arrange(namelist) %>%
  tally() %>%
  select(-one_of("n")) %>%
  rename(tid=namelist,item=eventlist) %>%
  as.data.frame()
#
third <- ddply(trans.list1,"tid",function(df) paste(df$item,collapse=","))
names(third)[2] <- "items"
head(third)
str(third)
#
itemsets3 <- third[,2,drop=F]
head(itemsets3)
str(itemsets3)
#
out.file <- tempfile() # temporary file name
write.csv(itemsets3,out.file,row.names=F,quote=F)
git.trans3 <- read.transactions(out.file,format="basket",sep=",",skip=1)
file.remove(out.file) # remove the temporary file
class(git.trans3)
head(git.trans3)
inspect(git.trans3[3]) # the 3rd transaction
summary(git.trans3)
#Frequent
freq3 <- apriori(git.trans3, 
                 parameter=list(supp=0.01,target="rules"))
summary(freq3)
freq3=sort(freq3,decreasing=T,by="lift")
inspect(freq3)
#maximal
Max3 <- is.maximal(freq3)
Max3
#
rules <- apriori(git.trans3, 
                 parameter=list(supp=0.001,conf=0.8,target="rules"))
summary(rules)
inspect(sort(rules,by="lift"))
#18:00~24:00
trans.list1 <- date4 %>%
  group_by(namelist,eventlist) %>%
  arrange(namelist) %>%
  tally() %>%
  select(-one_of("n")) %>%
  rename(tid=namelist,item=eventlist) %>%
  as.data.frame()
#
fourth <- ddply(trans.list1,"tid",function(df) paste(df$item,collapse=","))
names(fourth)[2] <- "items"
head(fourth)
str(fourth)
#
itemsets4 <- fourth[,2,drop=F]
head(itemsets4)
str(itemsets4)
#
out.file <- tempfile() # temporary file name
write.csv(itemsets4,out.file,row.names=F,quote=F)
git.trans4 <- read.transactions(out.file,format="basket",sep=",",skip=1)
file.remove(out.file) # remove the temporary file
class(git.trans4)
head(git.trans4)
inspect(git.trans4[3]) # the 3rd transaction
summary(git.trans4)
#Frequent
freq4 <- apriori(git.trans4, 
                 parameter=list(supp=0.01,target="rules"))
summary(freq4)
freq4=sort(freq4,decreasing=T,by="lift")
inspect(freq4)
#maximal
Max4 <- is.maximal(freq4)
#
rules <- apriori(git.trans4, 
                 parameter=list(supp=0.0009,target="rules"))
summary(rules)
inspect(sort(rules,by="lift"))