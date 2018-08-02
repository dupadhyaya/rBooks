###################################
# Code for: R for Marketing Research and Analytics, Chapter 6
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
# This file contains scripts used in Chapter 6 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# R code for Chapter 6 -- assessing differences between groups (continued)


#### quick way to load the data
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

### OR, if saved locally
load("~/segdf-Rintro-Ch5.RData")
summary(seg.df)

### OR simulate it following the code in Chapter 5



##### Statistical models and tests
### chi-square tests

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab <- tmp.tab/5
tmp.tab
chisq.test(tmp.tab)

# one way chi-square in our data
chisq.test(table(seg.df$Segment))

# two-way chi-square
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# two-way chi-square without correction (matches traditional formula)
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=FALSE)

# two-way with simulation to establish p value
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=TRUE, B=10000)


### binomial
binom.test(12, 20, p=0.5)
binom.test(120, 200, p=0.5)

sum(dbinom(8:12, 20, 0.5))

# agresti-coull might be more applicable for small N
# install.packages("binom")
library(binom)
binom.confint(12, 20, method="ac")

binom.confint(0, 20, method="ac")

### t-test

# first do some plotting to check income
hist(seg.df$income)
with(seg.df, hist(income[ownHome=="ownYes"]))
with(seg.df, hist(income[ownHome=="ownNo"]))

# then the t-tests
t.test(income ~ ownHome, data=seg.df)

t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))


### ANOVA
seg.aov.own <- aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)

seg.aov.seg <- aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

# two-way aov
anova(aov(income ~ Segment + ownHome, data=seg.df))

anova(aov(income ~ Segment * ownHome, data=seg.df))

# compare models
anova(aov(income ~ Segment, data=seg.df),
      aov(income ~ Segment + ownHome, data=seg.df))


### Visualize ANOVA group means

# install.packages("multcomp")
library(multcomp)

# create an aov model. problem for glht() plotting because of the intercept term
seg.aov <- aov(income ~ Segment, data=seg.df)
glht(seg.aov)

# make new AOV without intercept, as a *convenience for plotting* (not for modeling)
# it helps with plotting because it keeps all segments on same scale
seg.aov <- aov(income ~ -1 + Segment, data=seg.df)

glht(seg.aov)

par(mar=c(6,10,2,2))   # adjusts margins to preserve axis labels
plot(glht(seg.aov), 
     xlab="Income", main="Average Income by Segment (95% CI)")


### stepwise ANOVA

seg.aov.step <- step(aov(income ~ ., data=seg.df))
anova(seg.aov.step)


# clean up -- not shown in book
rm(seg.aov.own, seg.aov.seg, seg.aov.step, seg.aov)



#### *Bayesian ANOVA
#
# install.packages("BayesFactor")

set.seed(96761)
library(BayesFactor)
seg.bf1 <- lmBF(income ~ Segment, data=seg.df)
seg.bf2 <- lmBF(income ~ Segment + ownHome, data=seg.df)
seg.bf1 / seg.bf2

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000)

# plot the trace for posterior draw chain
plot(seg.bf.chain[, 1:6])   # note console: may need to hit <Return> to see all

summary(seg.bf.chain)

head(seg.bf.chain)

seg.bf.chain[1:4, 1:5]
seg.bf.chain[1:4, 2:5] + seg.bf.chain[1:4, 1]

seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
seg.bf.ci <- t(apply(seg.bf.chain.total, 2, 
                     quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf.ci


### plot the credible intervals
# ggplot2 version
# install.packages("ggplot2")
library(ggplot2)

# make a data frame from the posterior summary
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)

# basic plot with segment, 50% and 95% limits
p <- ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
# add the points and error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Income")

# plot it, adding a title, and flipping to horizontal

p + ggtitle("95% CI for Mean Income by Segment") + coord_flip()




