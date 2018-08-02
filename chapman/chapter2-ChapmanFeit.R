###################################
# Code for: R for Marketing Research and Analytics, Chapter 2
#
# Authors:  Chris Chapman               Elea McDonnell Feit
#           cnchapman+rbook@gmail.com   efeit@drexel.edu
#
# Copyright 2015, Springer 
#
# Last update: January 7, 2015
# Version: 1.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#
# You may obtain a copy of the License at
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#################################################################
# BRIEF HOW TO USE
# This file contains scripts used in Chapter 2 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# chapter 2

x <- c(2, 4, 6, 8)
x

# a brief tour of R

### START TOUR
# some setup
install.packages(c("lavaan", "semPlot", "corrplot", "multcomp"))

# Load the data
# Long version:
#   satData <- read.csv("http://r-marketing.r-forge.r-project.org/data/rintro-chapter2.csv")
satData <- read.csv("http://goo.gl/UDv12g")

# convert the Segment to a factor (categorical) variable
satData$Segment <- factor(satData$Segment)
head(satData)
summary(satData)

# correlation plot
library(corrplot)
corrplot.mixed(cor(satData[, -3]))

# product satisfaction by segment
aggregate(iProdSAT ~ Segment, satData, mean)

# ANOVA
sat.anova <- aov(iProdSAT ~ -1 + Segment, satData)
summary(sat.anova)

# plot the ANOVA estimates
library(multcomp)
par(mar=c(4,8,4,2))
plot(glht(sat.anova))

# define a structural model
satModel <- "SAT =~ iProdSAT + iSalesSAT
             REC =~ iProdREC + iSalesREC
             REC ~  SAT "

# fit the structural model
library(lavaan)
sat.fit <- cfa(satModel, data=satData)
summary(sat.fit, fit.m=TRUE)

# plot the structural model
library(semPlot)
semPaths(sat.fit, what="est", 
         residuals=FALSE, intercepts=FALSE, nCharNodes=9)

### END TOUR



# Language Fundamentals
x <- c(2, 4, 6, 8)
x
X   # different than "x" and likely will produce an error

x <- c(2, 4, 6, 8)   # start a cheer

# basic objects

# vectors
xNum  <- c(1, 3.14159, 5, 7)
xLog  <- c(TRUE, FALSE, TRUE, TRUE)
xChar <- c("foo", "bar", "boo", "far")
xMix  <- c(1, TRUE, 3, "Hello, world!") 
xNum

x2 <- c(x, x)
x2
summary(xNum)
summary(xChar)

xNum[2]
x2 + 1
x2 * pi 

(x+cos(0.5)) * x2

length(x)
length(x2)

c(1, 2, 3.5)
xMix
xNum[1]
xMix[1]
xNum[1] + 1

xMix[1] + 1    # error
as.numeric(xMix[1])+1

str(xNum)
str(xChar)
str(xMix)

# more on vectors and indexing

xSeq <- 1:10
xSeq
1:5*2
1:(5*2)

xNum
xNum[2:4]
myStart <- 2
xNum[myStart:sqrt(myStart+7)]

seq(from=-5, to=28, by=4)
rep(c(1,2,3), each=3)
rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1))

xSeq
xSeq[-5:-7]

1:300
1001:1300

xNum[2:4]
xSub <- xNum[2:4]
xSub

xNum
xNum[c(FALSE, TRUE, TRUE, TRUE)]

xNum[xNum > 3]


# missing and interesting values

my.test.scores <- c(91, NA, NA)

mean(my.test.scores)
max(my.test.scores)
mean(my.test.scores, na.rm=TRUE)
max(my.test.scores, na.rm=TRUE)

mean(na.omit(my.test.scores))
is.na(my.test.scores)
my.test.scores[!is.na(my.test.scores)]
my.test.scores <- c(91, -999, -999)
mean(my.test.scores)
my.test.scores[my.test.scores < -900] <- NA
mean(my.test.scores, na.rm=TRUE)
log(c(-1,0,1))


# lists

str(xNum)
str(xChar)

xList <- list(xNum, xChar)
xList

str(xList)

summary(xList[[1]])

lapply(xList, summary)
xList <- list(xNum, xChar)
names(xList) <- c("itemnum", "itemchar")     # method 1
xList <- list(itemnum=xNum, itemchar=xChar)  # method 2
names(xList)

xList[[1]]
xList$itemnum
xList[["itemnum"]]
# data frames

x.df <- data.frame(xNum, xLog, xChar)
x.df
 
x.df[2,1]
x.df[1,3]

x.df <- data.frame(xNum, xLog, xChar, stringsAsFactors=FALSE)
x.df[1,3]

x.df[2, ]  # all of row 2
x.df[ ,3]  # all of column 3

x.df[2:3, ] 
x.df[ ,1:2] 
x.df[-3, ]  # omit the third observation
x.df[, -2]  # omit the second column
str(x.df[2,1])
str(x.df[, 2])
str(x.df[c(1,3), ])    # use c() to get rows 1 and 3 only
x.df$xNum

# create more interesting data

# warning!!
rm(list=ls())    # caution, deletes all objects; see below


store.num <- factor(c(3, 14, 21, 32, 54))   # store id
store.rev <- c(543, 654, 345, 678, 234)     # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34)       # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
(store.df <- data.frame(store.num, store.rev, store.visits,
                        store.manager, stringsAsFactors=F))  # F = FALSE

store.df$store.manager
mean(store.df$store.rev)
cor(store.df$store.rev, store.df$store.visits)

summary(store.df)


# loading and saving data

save(store.df, file="store-df-backup.RData")
rm(store.df)     # caution, first ensure 'save' worked
mean(store.df$store.rev)    # error
load("store-df-backup.RData")
mean(store.df$store.rev)     # works now
save(list=c("store.df","store.visits"), file="store-df-backup.RData")

store.df <- 5
store.df
load("store-df-backup.RData")
store.df

# Works on Windows:
save(store.df, file="C:\\Documents and Settings\\user\\My Documents\\R\\store-df-backup.RData")

# Works on Mac OSX, Linux and Windows:
save(store.df, file="~/Documents/R/store-df-backup.RData")

getwd()
setwd("~/Documents/R")   # tilde is handled on UNIX-like systems
getwd()

save.image()    # saves .RData
save.image("mywork.RData")

load("mywork.RData")

list.files()

# warning: dangerous!
file.remove("mywork.RData", "store-df-backup.RData")

write.csv(store.df, row.names=FALSE)
write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")  # "file=" is optional
 
store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)  # "file=" is optional
store.df2$store.num <- factor(store.df2$store.num)

store.df == store.df2

all.equal(store.df, store.df2)
rm(store.df2)


####
#### Functions
se <- function(x) { sd(x) / sqrt(length(x)) }
se(store.df$store.visits)
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)

se(store.df$store.manager)

se <- function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x)   # standard deviation
  tmp.N  <- length(x)  # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N)   # std error of the mean
  return(tmp.se)
}


####
#### Language structures
x <- -2:2
log(x)                     # warning, can't log() negative numbers
ifelse(x > 0, x, NA)       # replace non-positive values with NA
log(ifelse(x > 0, x, NA))  # no warning now

my.data <- matrix(runif(100), ncol=5)   # 100 random numbers in 5 columns
apply(my.data, 2, median) / 2

halfmedian <- function (x) { median(x) / 2 }
apply(my.data, 2, halfmedian)

apply(my.data, 2, function(x) { median(x) / 2 } )


ls()
rm(store.num)
rm(list=c("store.rev","store.visits"))
rm(list=ls(pattern="store"))

# Warning!! putting this inside an "if (FALSE)" block on purpose
if (FALSE) {
  rm(list=ls())   # warning! deletes all objects in memory (except hidden ones)
}


