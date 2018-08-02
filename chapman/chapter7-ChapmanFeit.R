###################################
# Code for: R for Marketing Research and Analytics, Chapter 7
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
# This file contains scripts used in Chapter 7 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# Chapter 7

####### Alternative to load data if you want to skip simulation
sat.df <- read.csv("http://goo.gl/HKnl74")
# or
sat.df <- read.csv("http://r-marketing.r-forge.r-project.org/data/rintro-chapter7.csv")
#######                   
str(sat.df)


####
# Simulate satisfaction data
set.seed(08226)
nresp <- 500 # number of survey respondents
halo <- floor(rnorm(n=nresp, mean=0, sd=5))
rides <- floor(halo + rnorm(n=nresp, mean=80, sd=3)+7)
games <- floor(halo + rnorm(n=nresp, mean=70, sd=7)+10)
wait <- floor(halo + rnorm(n=nresp, mean=65, sd=10)+6)
clean <- floor(halo + rnorm(n=nresp, mean=85, sd=2)+4)
cor(rides, games)
distance <- rlnorm(n=nresp, meanlog=3, sdlog=1)
num.child <- sample(x=0:5, size=nresp, replace=TRUE, 
                    prob=c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05))
weekend <- as.factor(sample(x=c("yes", "no"), size=nresp, replace=TRUE, 
                            prob=c(0.5,0.5)))
overall <- floor(halo + 0.5*rides + 0.1*games + 0.3*wait + 0.2*clean + 
                 0.03*distance + 5*(num.child==0) + 0.3*wait*(num.child>0) + 
                 rnorm(n=nresp, mean=0, sd=7) - 54)

sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, overall)
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, 
   overall)

# Try it!: ls()



####
# Overview of linear regression
summary(sat.df)

# check basic data suitability
library(gpairs)

gpairs(sat.df)

# fix distance
sat.df$logdist <- log(sat.df$distance)
gpairs(sat.df)   ### NOT shown in book



# NOTE: if using RStudio, it can be helpful to "Clear All" plots if a plot
# appears too small or too large; 
# this is a known issue in RStudio with various packages such as corrplot
#

library(corrplot)

corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")

# Fitting a model with a single predictor
lm(overall~rides, data=sat.df)

-94.962 + 1.703*95

# Try it!: sd(m1$residuals)
m1 <- lm(overall~rides, data=sat.df)

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

str(m1)
m1$coefficients

summary(m1)


####
# confidence intervals by hand:
1.703 - 1.96*(0.106)
1.703 + 1.96*(0.106)

confint(m1)

cor(sat.df$overall, sat.df$rides)^2
# Try it: anova(m1)


####
#Assessing model fit
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y~x)
# Try it!: summary(toy.model)
plot(y~x)
abline(toy.model, col='blue')

plot(toy.model$fitted.values, toy.model$residuals)

par(mfrow=c(2,2))
plot(m1)


sat.df[c(57, 129, 295), ]


####
# Fitting a more complex model with lm()
# Try it!:
# cor(sat.df[2:7])
# library(car)
# scatterplotMatrix(sat.df)

m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)
# Try it!: plot(m2)

par(mfrow=c(1,1))

library(coefplot)
coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")


# Comparing Models
summary(m1)$r.squared
summary(m2)$r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
plot(sat.df$overall, fitted(m1), col='red',
     xlim=c(0,100), ylim=c(0,100),
     xlab="Actual Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col='blue')
legend("topleft", legend=c("model 1", "model 2"), 
       col=c("red", "blue"), pch=1)

anova(m1, m2)


####
# Prediction
coef(m2)["(Intercept)"] + coef(m2)["rides"]*100 + coef(m2)["games"]*100 + 
  coef(m2)["wait"]*100 + coef(m2)["clean"]*100 
coef(m2)%*%c(1,100, 100, 100, 100)
predict(m2, sat.df[1:10,])
fitted(m2)[1:10]


####
# standardizing
# by hand ...
(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)
scale(sat.df$rides)

# create sat.std as a standardized version of sat

sat.std <- sat.df[ , -3]  # sat but remove distance
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std)


####
# Handling factors
m3 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child, 
         data = sat.std)
summary(m3)

sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child.factor, 
         data=sat.std)
summary(m4)

sat.std$has.child <- factor(sat.std$num.child > 0)

m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child, 
         data=sat.std)
summary(m5)


####
# Interaction
m6 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + has.child + 
                   rides:has.child + games:has.child + wait:has.child +
                   clean:has.child + 
                   rides:weekend + games:weekend + wait:weekend + clean:weekend, 
         data=sat.std)

summary(m6)


####
# reduced model
m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child + 
                   wait:has.child,
         data=sat.std)
summary(m7)

# TRY: coef(m7)["wait"] + coef(m7)["wait:has.childTRUE"]

#Try it!
# anova(m5, m7)
# plot(m7)

library(coefplot)

coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")


####
# Bayesian Linear Model
library(MCMCpack)
m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + logdist + 
                          has.child + wait:has.child,
                        data=sat.std)
summary(m7.bayes)

