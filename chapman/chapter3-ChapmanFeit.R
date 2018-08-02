###################################
# Code for: R for Marketing Research and Analytics, Chapter 3
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
# This file contains scripts used in Chapter 3 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################



# chapter 3

############
# alternative command to download the data. 
# ==> But don't do this! See the book and SIMULATE the data step by step below
# Long URL:
# store.df <- read.csv(paste("http://r-marketing.r-forge.r-project.org/",
#                           "data/rintro-chapter3.csv", sep=""))
# Short URL:
store.df <- read.csv("http://goo.gl/QPDdMl")  # only if you skip the simulation
summary(store.df)
############


############
# create the data

k.stores <- 20    # 20 stores
k.weeks <- 104    # 2 years of data each

# create a data frame of initially missing values to hold the data
store.df <- data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", 
                     "p1price", "p2price", "p1prom", "p2prom", "country")

dim(store.df)

store.num <- 101:(100+k.stores)
(store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
                rep("JP", 4), rep("AU", 1), rep("CN", 2)))
length(store.cty)    # make sure the country list is the right length


store.df$storeNum <- rep(store.num, each=k.weeks)
store.df$country  <- rep(store.cty, each=k.weeks)
rm(store.num, store.cty)    # clean up

(store.df$Week <- rep(1:52, times=k.stores*2))

# try the inner parts of the next line to figure out how we use rep()
(store.df$Year  <- rep(rep(1:2, each=k.weeks/2), times=k.stores))
str(store.df)

store.df$storeNum <- factor(store.df$storeNum)
store.df$country  <- factor(store.df$country)
str(store.df)

head(store.df)   # defaults to 6 rows
head(store.df, 120)  # 120 rows is enough to check 2 stores
tail(store.df, 120)  # make sure end looks OK too


# set random seed to make the random sequences replicable
set.seed(98250)  # a favorite US postal code

# promotion status, using binomial distribution, rbinom()
store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.1)  # 10% promoted
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15) # 15%
head(store.df)

# prices
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), 
                           size=nrow(store.df), replace=TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), 
                           size=nrow(store.df), replace=TRUE)
head(store.df)

# sales data, using poisson (counts) distribution, rpois()
# first, the default sales in the absence of promotion
tmp.sales1 <- rpois(nrow(store.df), lambda=120)  # lambda = mean sales per week
tmp.sales2 <- rpois(nrow(store.df), lambda=100)  # lambda = mean sales per week

# scale sales according to the ratio of log(price)
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# final sales get a 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom*0.4))
head(store.df)

# install.packages("car")  #only run once
library(car)
some(store.df, 10)

# command to write it out to a CSV:
# write.csv(store.df, file="rintro-chapter3.csv",row.names=FALSE)
#

############
# descriptives

table(store.df$p1price)

p1.table <- table(store.df$p1price)
p1.table
str(p1.table)


plot(p1.table)

table(store.df$p1price, store.df$p1prom)
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])



# functions
min(store.df$p1sales)
max(store.df$p2sales)

mean(store.df$p1prom)
median(store.df$p2sales)

var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)

quantile(store.df$p1sales, probs=c(0.25,0.50,0.75))   # interquartile range

quantile(store.df$p1sales, probs=c(0.05, 0.95))  # central 90%
quantile(store.df$p1sales, probs=0:10/10)

## Build a summary
mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

## Descriptive summaries
summary(store.df)
summary(store.df$Year)
summary(store.df, digits=2)

# install.packages("psych")  #only run once
library(psych)
describe(store.df)
describe(store.df[ , c(2, 4:9)])


############
# apply()
apply(store.df[, 2:9], MARGIN=2, FUN=mean)
apply(store.df[, 2:9], 1, mean)
apply(store.df[, 2:9], 2, sum)
apply(store.df[, 2:9], 2, sd)
apply(store.df[, 2:9], 2, function(x) { mean(x) - median(x) } )

## creating a summary data frame using apply()
mysummary2.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary2.df) <- c("Median Sales", "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5] # names from the data frame
mysummary2.df[, "Median Sales"] <- apply(store.df[, 4:5], 2, median)
mysummary2.df[, "IQR"]          <- apply(store.df[, 4:5], 2, IQR)
mysummary2.df



############
# single variable visualization

hist(store.df$p1sales)



## prettier version

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",    # add labels
     xlab="Product 1 Sales (Units)",
     ylab="Count" )           


hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,             # more columns 
     col="lightblue")       # color the bars


# relabeling the x axis

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",             # it's no londer the count!
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want


# adding curves to the histogram

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want


lines(density(store.df$p1sales, bw=10),    # "bw= ..." adjusts the smoothing
      type="l", col="darkred", lwd=2)      # lwd = line width


############
# boxplots and stripcharts

## boxplot

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)


boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
     ylab="Store", xlab="Weekly unit sales", las=1,
     main="Weekly Sales of P2 by Store")

# using data=...

boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
     ylab="P2 promoted in store?", xlab="Weekly sales",
     main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))



### qq check for normality
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

### is more normal with log()
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))


### ecdf cumulative distribution plot

plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab="Cumulative Proportion",
     xlab=c("P1 weekly sales, all stores", "90% of weeks sold <= 171 units"),
     yaxt="n")
axis(side=2, at=seq(0, 1, by=0.1), las=1, 
     labels=paste(seq(0,100,by=10), "%", sep=""))
# add lines for 90%
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)



### by() and aggregate()
by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)


############
# world map 

# aggregate the average per-store sales by country
p1sales.sum <- aggregate(store.df$p1sales, 
                         by=list(country=store.df$country), sum)

# install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)

# create map
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2", 
                                   nameJoinColumn = "country")

mapCountryData(p1sales.map, nameColumnToPlot="x", 
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"), 
               catMethod="fixedWidth", addLegend=FALSE)


