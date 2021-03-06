####
###
###https://github.com/PacktPublishing/Learning-Quantitative-Finance-with-R/blob/master/Chapter01/Chapter1_Code
###Chap1
#Install Package
.libPaths()
library()
install.packages("Package")
install.packages("ggplot2")
install.packages("forecast")
install.packages("ggplot2", lib="/data/Rpackages/")
library(ggplot2, lib.loc="/data/Rpackages/")
##Data Types
a <- "hello"
print(class(a))
a <- 2.5
print(class(a))
a <- 6L
print(class(a))
a <- 1 + 2i
print(class(a))
a <- TRUE
print(class(a))
a<-"Quantitative"
b<-"Finance"
c(a,b)
Var<-c(1,2,3)
Var
List1 = list(c(4,5,6),"Hello", 24.5)
print(List1)
print(List1[2])
list1 <- list(5,6,7)
list2 <- list("a","b","c")
Combined_list <-c(list1,list2)
print(Combined_list)
M <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
print(M)
a <- array(c(4,5),dim = c(3,3,2))
print(a) 
a <-c(2,3,4,2,3)
fact <-factor(a)
print(fact)
print(nlevels(fact))
data <-data.frame(
  Name = c("Alex", "John", "Bob"),
  Age = c(18,20,23),
  Gender =c("M","M","M")
)
print(data)
print (getwd())
#setwd("")
setwd("C:/Users")
#Import, export of different data types
print (getwd())
setwd("E:/rWork/rProjects/fa/qfRbook1")
data<-read.csv("Sample.csv")
print(data)
print(is.data.frame(data))
write.csv(data,"result.csv")
output <- read.csv("result.csv")
print(output)


install.packages("xlsx")
any(grepl("xlsx",installed.packages()))
any(grepl("arulesViz555",installed.packages()))

#NOTWORKING----
library("xlsx")
install.packages('rJava')
library(rJava)
write.xlsx(output,"result.xlsx")
data <- read.xlsx("Sample.xlsx", sheetIndex = 1)
print(data)
output<-write.xlsx(data,"result.xlsx")
output<- read.csv("result.csv")
print(output)
# NOTWK----

URL <- "http://ichart.finance.yahoo.com/table.csv?s=^GSPC"
URL <- "http://finance.yahoo.com/table.csv?s=HCLTECH.NS"
snp <- as.data.frame(read.csv(URL))  # not working
head(snp)

URL <- "http://ichart.finance.yahoo.com/table.csv?s=SPY"
dat <- read.csv(URL)
dat$Date <- as.Date(dat$Date, "%Y-%m-%d")

URL <- "http://ichart.finance.yahoo.com/table.csv?s=^DJI"
dji <- as.data.frame(read.csv(URL))
head(dji)


###Write code expressions functions
4+5
4+5
4-5
4*5
10>5 
5>10 

print(mean(25:82))
print(sum(41:68))

findingSqrFunc<-function(value){
  for(j in 1:value){
    sqr<-j^2
    print(sqr)
  }
}
findingSqrFunc(4)

Function_test<-function(){
  for(i in 1:3){
    print(i*5)
  }
}
Function_test()


Function_test<-function(a,b,c){
  result<-a*b+c
  print(result)
}
Function_test(2,3,4)
Function_test(c=4,b=3,a=4)

###loops
x<-5
if(x>0)
{
  print(" I am Positive")
}

x<--5
if(x>0)
{
  print(" I am Positive")
}else
{
  print(" I am Negative")
}


Var <- c(3,6,8,9,11,16)
counter <- 0
for (val in Var) {
  if(val %% 2 != 0)  counter = counter+1
}
print(counter)


Var <- c("Hello")
counter <- 4
while (counter < 7) {
  print(Var)
  counter = counter+ 1
}

sample = matrix(c(1:10), nrow = 5 , ncol = 2)
apply(sample, 1,sum)

sapply(1:5, function(x) x^3)