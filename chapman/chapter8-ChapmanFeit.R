###################################
# Code for: R for Marketing Research and Analytics, Chapter 8
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
# This file contains scripts used in Chapter 8 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# Chapter 8


####
#### Dimensional Consumer Data
####

# load the data
brand.ratings <- read.csv("http://goo.gl/IQl8nc")

# check it out
head(brand.ratings)
tail(brand.ratings)

summary(brand.ratings)
str(brand.ratings)

# Rescaling the data
# center and Z-score it

# example
x <- 1:1000
x.sc <- (x - mean(x)) / sd(x)
summary(x.sc)

# transform the brand data
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
summary(brand.sc)

library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order="hclust")


# aggregate personality attributes by brand
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean

rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1]          # remove brand name column
brand.mean


#### basic visualization of the raw data

# heatmap of attribute by brand
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean), 
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          main="\n\n\n\n\nBrand attributes")


####
#### Principal components
####

# Simple example
# create simple correlated data
set.seed(98286)
xvar <- sample(1:10, 100, replace=TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace=TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace=TRUE)
my.vars <- cbind(xvar, yvar, zvar)

# visualize
plot(yvar ~ xvar, data=jitter(my.vars))

cor(my.vars)

# principal components
my.pca <- prcomp(my.vars)
summary(my.pca)
my.pca

cor(my.pca$x)   # components have zero correlation

# biplot
biplot(my.pca, scale=TRUE)


## PCA for brand ratings
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)

plot(brand.pc, type="l")

biplot(brand.pc)    # very dense!


# try again with just the means
brand.mean
brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)

biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1))


# compare brands
brand.mean["c", ] - brand.mean["e", ]

# towards unoccupied space between b/c & g/f
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]


#### Exploratory Factor Analysis

library(nFactors)
nScree(brand.sc[, 1:9])
eigen(cor(brand.sc[, 1:9]))

factanal(brand.sc[, 1:9], factors=2)
factanal(brand.sc[, 1:9], factors=3)


library(GPArotation)
(brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin"))

library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings, 
          col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")


# plot the structure
library(semPlot)
semPaths(brand.fa.ob, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)


# use regression scores
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin", 
                        scores="Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.sc$brand
head(brand.scores)

brand.fa.mean <- aggregate(. ~ brand, data=brand.scores, mean)
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c("Leader", "Value", "Latest")
brand.fa.mean


heatmap.2(as.matrix(brand.fa.mean), 
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand")


# 

#### Multidimensional scaling
# distance matrix
brand.dist <- dist(brand.mean)

# metric MDS
(brand.mds <- cmdscale(brand.dist))
# plot it
plot(brand.mds, type="n")
text(brand.mds, rownames(brand.mds), cex=2)


# non-metric MDS alternative 
brand.rank <- data.frame(lapply(brand.mean, function(x) ordered(rank(x))))
str(brand.rank)

library(cluster)
brand.dist.r <- daisy(brand.rank, metric="gower")

brand.mds.r <- isoMDS(brand.dist.r)

plot(brand.mds.r$points, type="n")
text(brand.mds.r$points, levels(brand.sc$brand), cex=2)


#### extra!
#
# the following is NOT in the book
# same general solution as PCA, but with clustering instead 
# this is a preview of the segmentation chapter
library(cluster)
clusplot(fanny(brand.dist, k=3), color=FALSE, shade=FALSE, 
         labels=3, lines=0, plotchar=FALSE,
         main="Brand perception groups")

