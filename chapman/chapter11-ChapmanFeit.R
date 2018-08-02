###################################
# Code for: R for Marketing Research and Analytics, Chapter 11
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
# This file contains scripts used in Chapter 11 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################

# R code for Chapter 11 -- segmentation



# load data that we first saw in Chapter 5
# ==> locally, only IF you saved it earlier; might need to change directory
load("~/segdf-Rintro-Ch5.RData")
seg.raw <- seg.df
seg.df  <- seg.raw[ , -7]     # a copy without the known segment assignments

# OR read it from the web site:
seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df  <- seg.raw[ , -7]     # a copy without the known segment assignments

summary(seg.df)



# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)


#### HCLUST

# distance example -- by hand
c(1,2,3) - c(2,3,2)
sum((c(1,2,3) - c(2,3,2))^2)
sqrt(sum((c(1,2,3) - c(2,3,2))^2))

# using dist()
dist(rbind(c(1,2,3), c(2,3,2)))

# distances using numeric columns of seg.df (first 5 observations)
d <- dist(seg.df[, c("age", "income", "kids")])
as.matrix(d)[1:5, 1:5]
rm(d)   # clean up, not in book

# now the real hclust() work
library(cluster)                  # daisy works with mixed data types
seg.dist <- daisy(seg.df)
# inspect some of the results
as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method="complete")

plot(seg.hc)

# zoom in on just part of it
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])

# check some of the proposed similarities
seg.df[c(101, 107), ]  # similar
seg.df[c(278, 294), ]  # similar
seg.df[c(173, 141), ]  # less similar

# examine cophenetic correlation
cor(cophenetic(seg.hc), seg.dist)

# see hclust's proposal for 4 groups
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")

# actually get 4 groups
seg.hc.segment <- cutree(seg.hc, k=4)     # membership vector for 4 groups
table(seg.hc.segment)

# what did hclust come up with?
seg.summ(seg.df, seg.hc.segment)

# plot this
plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), 
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))



#### K-MEANS
# convert factor variables to numeric (kmeans requires). OK b/c all are binary.
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)

set.seed(96743)
seg.k <- kmeans(seg.df.num, centers=4)

# inspect it
seg.summ(seg.df, seg.k$cluster)

# plot one of the variables
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")


# plot the result
library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")


#### MCLUST

# do mclust for segments
library(mclust)

###
# convert factor variables to numeric (mclust requires). OK b/c all are binary.
# these lines are the same as above for k-means [not repeated in book]
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)
###


# fit the model
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

# what if we estimate 4 clusters?
seg.mc4 <- Mclust(seg.df.num, G=4)
summary(seg.mc4)

# compare the two models
BIC(seg.mc, seg.mc4)

# examine the 3-cluster model
seg.summ(seg.df, seg.mc$class)


# plot the 3-cluster model
library(cluster)
clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")



#### poLCA
seg.df.cut <- seg.df
seg.df.cut$age    <- factor(ifelse(seg.df$age < median(seg.df$age), 1, 2))
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income),
                                   1, 2))
seg.df.cut$kids   <- factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2))
summary(seg.df.cut)

# create a model formula
seg.f <- with(seg.df.cut, 
              cbind(age, gender, income, kids, ownHome, subscribe)~1)

# fit the model
library(poLCA)
set.seed(02807)
seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)
seg.LCA4 <- poLCA(seg.f, data=seg.df.cut, nclass=4)

seg.LCA4$bic
seg.LCA3$bic


# examine the solutions
# 3 clusters
seg.summ(seg.df, seg.LCA3$predclass)
table(seg.LCA3$predclass)

clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=3)")


# 4 clusters
seg.summ(seg.df, seg.LCA4$predclass)
table(seg.LCA4$predclass)

clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")



# compare 3-cluster and 4-cluster solutions
table(seg.LCA3$predclass, seg.LCA4$predclass)

library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)

# compare random assignment to LCA4
set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE)
adjustedRandIndex(random.data, seg.LCA4$predclass)


# compare to known segments
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)



#### CLASSIFICATION

#### NAIVE BAYES
# install.packages("e1071")   # if needed

set.seed(04625)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.cases, ]
seg.df.test  <- seg.raw[-train.cases, ]

library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train))
(seg.nb.class <- predict(seg.nb, seg.df.test))

# frequencies in predicted data
prop.table(table(seg.nb.class))

# plot it
clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, 
         main="Naive Bayes classification, holdout data")


# compare to known segments (which we can do with this test data)
mean(seg.df.test$Segment==seg.nb.class)

# adjusted for chance
library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)

table(seg.nb.class, seg.df.test$Segment)

# summary data for proposed segments in the test data
seg.summ(seg.df.test, seg.nb.class)
# summary data for the known segments in the test data
seg.summ(seg.df.test, seg.df.test$Segment)

# predict raw probabilities
predict(seg.nb, seg.df.test, type="raw")


#### random forest
library(randomForest)
set.seed(98040)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000))


# predict the test data for random forest
seg.rf.class <- predict(seg.rf, seg.df.test)

# plot the solution
library(cluster)

clusplot(seg.df.test[, -7], seg.rf.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Random Forest classification, holdout data")



# get the individual prediction distribution
seg.rf.class.all <- predict(seg.rf, seg.df.test, predict.all=TRUE)

# look at the distribution for the first 5 test data cases
apply(seg.rf.class.all$individual[1:5, ], 1, table) / 3000

# summaries for the proposed and actual segments
seg.summ(seg.df.test, seg.rf.class)
seg.summ(seg.df.test, seg.df.test$Segment)

# confusion matrix in test data
mean(seg.df.test$Segment==seg.rf.class)
table(seg.df.test$Segment, seg.rf.class)

library(mclust)
adjustedRandIndex(seg.df.test$Segment, seg.rf.class)


### random forest variable importance
set.seed(98040)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000,
                        importance=TRUE))

importance(seg.rf)

varImpPlot(seg.rf, main="Variable importance by segment")

library(gplots)
library(RColorBrewer)


heatmap.2(t(importance(seg.rf)[ , 1:4]), 
          col=brewer.pal(9, "Blues"), 
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
          )



#### predict subscription status

#### using random forest

set.seed(92118)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.raw[train.cases, ]
sub.df.test  <- seg.raw[-train.cases, ]


# see how differentiated the subscribers are, in the training data

clusplot(sub.df.train[, -6], sub.df.train$subscribe, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Subscriber clusters, training data")


library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000))

# try again with more trees, and balanced classes using sampsize
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000, 
                       sampsize=c(25, 25)) )

# predict the holdout data
sub.rf.sub <- predict(sub.rf, sub.df.test)
# confusion matrix
table(sub.rf.sub, sub.df.test$subscribe)

library(mclust)
adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)

library(psych)
cohen.kappa(cbind(sub.rf.sub, sub.df.test$subscribe))




