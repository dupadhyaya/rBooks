###################################
# Code for: R for Marketing Research and Analytics, Chapter 5
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
# This file contains scripts used in Chapter 5 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# R code for Chapter 5 -- assessing differences between groups


#### quick alternative to load the data
#### ... but better is to create the data as below
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)


#### Create the data

# Names of the variables we will define for each segment
segVars <- c("age", "gender", "income", "kids", "ownHome", "subscribe")

# the data type for each segment
segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")

# names of the segments
segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")

# the size of each segment (N)
segSize <- c(100, 50, 80, 70)

# the means for each variable for each segment
segMeans <- matrix( c(
  40, .5, 55000, 2, .5, .1,
  24, .7, 21000, 1, .2, .2,
  58, .5, 64000, 0, .7, .05,
  36, .3, 52000, 2, .3, .2  ), ncol=length(segVars), byrow=TRUE)

# the standard deviations for each segment (NA = not applicable for the variable)
segSDs <- matrix( c(
  5, NA, 12000, NA, NA, NA,
  2, NA,  5000, NA, NA, NA,
  8, NA, 21000, NA, NA, NA,
  4, NA, 10000, NA, NA, NA  ), ncol=length(segVars), byrow=TRUE)


#### digression to learn about for() loops
for (i in 1:10) { print(i) }

(i.seq <- rep(sqrt(seq(from=2.1, to=6.2, by=1.7)), 3))
for (i in i.seq ) { print(i) }

for (i in c("Hello ","world, ","welcome to R!")) { cat(i) }

for (i in 1:length(i.seq)) { cat("Entry", i, "=", i.seq[i], "\n") }

for (i in seq_along(i.seq)) { cat("Entry", i, "=", i.seq[i], "\n") }

i.seq <- NULL
# maybe we have a bunch of other code, and then ...
for (i in 1:length(i.seq)) { print (i) }     # bad

i.seq <- NULL
# maybe we have a bunch of other code, and then ...
for (i in seq_along(NULL)) { print (i) }     # better


#### if statements
x <- 1:5
if (x > 1) {      # bad code -- will produce warning!
  print ("hi") 
} else { 
  print ("bye") 
}

ifelse(x > 1, "hi", "bye")

fn.hi  <- function() { "hi" }
fn.bye <- function() { "bye" }
ifelse(x > 1, fn.hi(), fn.bye() )


#### return to data generation

# make sure we're starting our dataset from a known state
seg.df <- NULL
set.seed(02554)

# iterate over all the segments and create data for each
for (i in seq_along(segNames)) {    
  cat(i, segNames[i], "\n")

  # create an empty matrix to hold this particular segment's data
  this.seg <- data.frame(matrix(NA, nrow=segSize[i], ncol=length(segVars)))

  # within a segment, iterate over the variables and draw appropriate random data
  for (j in seq_along(segVars)) {    # and iterate over each variable
    if (segVarType[j] == "norm") {   # draw random normals
      this.seg[, j] <- rnorm(segSize[i], mean=segMeans[i, j], sd=segSDs[i, j])
    } else if (segVarType[j] == "pois") {    # draw counts
      this.seg[, j] <- rpois(segSize[i], lambda=segMeans[i, j])
    } else if (segVarType[j] == "binom") {   # draw binomials
      this.seg[, j] <- rbinom(segSize[i], size=1, prob=segMeans[i, j])
    } else {
      stop("Bad segment data type: ", segVarType[j])
    }
  }
  # add this segment to the total dataset
  seg.df <- rbind(seg.df, this.seg)     
}
# make the data frame names match what we defined
names(seg.df) <- segVars
# add segment membership for each row
seg.df$Segment   <- factor(rep(segNames, times=segSize))
# convert the binomial variables to nicely labeled factors
seg.df$ownHome   <- factor(seg.df$ownHome, labels=c("ownNo", "ownYes"))
seg.df$gender    <- factor(seg.df$gender, labels=c("Female", "Male"))
seg.df$subscribe <- factor(seg.df$subscribe, labels=c("subNo", "subYes"))

# check the data and confirm it
summary(seg.df)

### save the data
### adjust for your system and preference if you wish
save(seg.df, file="~/segdf-Rintro-Ch5.RData")

# write.csv(seg.df, "rintro-chapter5.csv", row.names=FALSE)

###### END of data creation section



# descriptives

mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe=="subNo"])

by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

aggregate(seg.df$income, list(seg.df$Segment), mean)

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
library(car)
some(seg.df)

seg.df$Segment
seg.income.mean[seg.df$Segment, ]
seg.income.mean[seg.df$Segment, 2]

# clear that variable
seg.df$segIncome <- NULL


#### formula version

aggregate(income ~ Segment, data=seg.df, mean)

##########
# two-way data aggregation
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)

agg.data <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
agg.data[2, ]
agg.data[2, 3]

# Count of factor level occurence by factor
with(seg.df, table(Segment, ownHome))
with(seg.df, table(kids, Segment))

# total of variables by factor
xtabs(kids ~ Segment, data=seg.df)

aggregate(kids ~ Segment, data=seg.df, sum)

seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)
colSums(seg.tab*0:7)



#### visualize counts by group


library(lattice)


# histogram by 1 factor
histogram(~subscribe | Segment, data=seg.df)

# counts instead of proportions, and some visual options
histogram(~subscribe | Segment, data=seg.df, type="count", 
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))


# histogram by 2 factors
histogram(~subscribe | Segment + ownHome, data=seg.df)

# use prop.table to get just positive proportion
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)

barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ], 
          xlab="Subscriber proportion by Segment", col="darkolivegreen")



#### visualize continuous data by group

## bar chart for continuous variable, the "spreadsheet" way to graph it
# aggregate our data
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
# plot it
library(lattice)

barchart(income~Segment, data=seg.mean, col="grey")

seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
# then plot it
barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50"))   # try rainbow, topo.colors, heat.colors, cm.colors
)


## better = boxplot for continuous variable

# base graphics way to do this

boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)


# lattice gives more options, especially for multiway breakouts ("conditioning")
library(lattice)

bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")

# add conditioning variable
bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, 
       xlab="Income")

