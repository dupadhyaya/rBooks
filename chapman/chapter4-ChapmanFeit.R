###################################
# Code for: R for Marketing Research and Analytics, Chapter 4
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
# This file contains scripts used in Chapter 4 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# Chapter 4

########
# Simulate CRM data

# optional instead of creating data:
#   cust.df <- read.csv(paste("http://r-marketing.r-forge.r-project.org/",
#                          "data/rintro-chapter4.csv", sep=""))
# OR
# cust.df <- read.csv("http://goo.gl/PmPkaG")
#
# and then:
#   cust.df$cust.id <- factor(cust.df$cust.id)

set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id=factor(1:ncust))

cust.df$age <- rnorm(n=ncust, mean=35, sd=5)
cust.df$credit.score <- rnorm(n=ncust, mean=3*cust.df$age+620, sd=50)
cust.df$email <- factor(sample(c("yes", "no"), size=ncust, replace=TRUE, 
                            prob=c(0.8, 0.2)))
cust.df$distance.to.store <- exp(rnorm(n=ncust, mean=2, sd=1.2)) 
summary(cust.df)

# Try it!: plot(hist(distance.to.store))

cust.df$online.visits <- rnbinom(ncust, size=0.3, 
                         mu = 15 + ifelse(cust.df$email=="yes", 15, 0) 
                              - 0.7 * (cust.df$age-median(cust.df$age))) 

cust.df$online.trans <- rbinom(ncust, size=cust.df$online.visits, prob=0.3)
cust.df$online.spend <- exp(rnorm(ncust, mean=3, sd=0.1)) * 
                                 cust.df$online.trans

cust.df$store.trans <- rnbinom(ncust, size=5, 
                               mu=3 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * 
                                 cust.df$store.trans
summary(cust.df)

sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
summary(sat.overall)

sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))

sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))

no.response <- as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)

rm(ncust, sat.overall, sat.service, sat.selection, no.response)

str(cust.df)

######## data simulation is complete!


# Basic scatterplot

plot(x=cust.df$age, y=cust.df$credit.score)

plot(cust.df$age, cust.df$credit.score, 
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900), 
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Customer Credit Score ")
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

# Scatterplot with skewed variables and color-coded factors
plot(cust.df$store.spend, cust.df$online.spend, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.7)

hist(cust.df$store.spend, 
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     main="Customers as of June 2014", 
     xlab="Prior 12 months online sales ($)", 
     ylab="Count of customers")

my.col <- c("black", "green3") 
my.pch <- c(1, 19) # R's symbols for solid and open circles (see ?points)

head(cust.df$email)
as.numeric(head(cust.df$email))
my.col[as.numeric(head(cust.df$email))]
my.col[head(cust.df$email)]

#Try it!
plot(cust.df$store.spend, cust.df$online.spend,
     col=as.numeric(cust.df$email))

plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email], pch=my.pch[cust.df$email], 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
       col=my.col, pch=my.pch)

# Try it!

plot(cust.df$store.spend + 1, cust.df$online.spend + 1,
     log="xy", cex=0.7,
     col=my.col[cust.df$email], pch=my.pch[cust.df$email],
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
       col=my.col, pch=my.pch)


# Multi-panel plots
par(mfrow=c(2, 2))
plot(cust.df$distance.to.store, cust.df$store.spend, main="store")
plot(cust.df$distance.to.store, cust.df$online.spend, main="online")
plot(cust.df$distance.to.store, cust.df$store.spend+1, log="xy", 
     main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend+1, log="xy", 
     main="online, log")


# Scatterplot matrix
pairs(formula = ~ age + credit.score + email +
                  distance.to.store + online.visits + online.trans + 
                  online.spend + store.trans + store.spend,
      data=cust.df)

pairs(cust.df[ , c(2:10)])


library(car)   # install if needed
scatterplotMatrix(formula = ~ age + credit.score + email +
                    distance.to.store + online.visits + online.trans + 
                    online.spend + store.trans + store.spend, 
                  data=cust.df, diagonal="histogram")


#install.packages("gpairs")  # only run once
library(gpairs)

gpairs(cust.df[ , c(2:10)])



# Try it!: gpairs(cbind(as.numeric(cust.df$email), cust.df[,6:8]))

# Correlations
cov(cust.df$age, cust.df$credit.score)

cor(cust.df$age, cust.df$credit.score)

cov(cust.df$age, cust.df$credit.score)/
  (sd(cust.df$age)*sd(cust.df$credit.score))

cor.test(cust.df$age, cust.df$credit.score)

# Correlation matrix
cor(cust.df[, c(2, 3, 5:12)])

# Try it!: cor(cust.df[,3:12], use="complete.obs")

library(corrplot)    # for correlation plot
library(gplots)      # for color interpolation


# NOTE: if using RStudio, it can be helpful to "Clear All" plots if one
# appears too small or too large; 
# this is a remnant of par(mfrow=..) settings as above and similar settings
#
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))


# Transformations

set.seed(49931)
x <- runif(1000, min=-10, max=10)
cor(x, x^2)

# Try it!  :  plot(x,x^2)

cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)

cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

# see note above about doing "Clear All" plots in RStudio if plot is odd
#
plot(cust.df$distance.to.store, cust.df$store.spend)

plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

library(car)
powerTransform(cust.df$distance.to.store)

lambda <- coef(powerTransform(1/cust.df$distance.to.store))
bcPower(cust.df$distance.to.store, lambda)


par(mfrow=c(1,2))
hist(cust.df$distance.to.store, 
     xlab="Distance to Nearest Store", ylab="Count of Customers", 
     main="Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda),
     xlab="Box-Cox Transform of Distance", ylab="Count of Customers", 
     main="Transformed Distribution")

powerTransform(cust.df$age)

l.dist  <- coef(powerTransform(cust.df$distance.to.store))
l.spend <- coef(powerTransform(cust.df$store.spend+1))

cor(bcPower(cust.df$distance.to.store, l.dist), 
    bcPower(cust.df$store.spend+1, l.spend))

# Polychoric correlations
plot(cust.df$sat.service, cust.df$sat.selection, 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")

plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection), 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")

resp <- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp]) 

library(psych)
polychoric(cbind(cust.df$sat.service[resp], 
                 cust.df$sat.selection[resp]))
     

