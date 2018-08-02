###################################
# Code for: R for Marketing Research and Analytics, Chapter 10
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
# This file contains scripts used in Chapter 10 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################


# R code for Chapter 10 -- structural modeling


##### preliminaries

### load data if you prefer not to simulate it as below
piesSimData <- read.csv("http://goo.gl/yT0XwJ")
summary(piesSimData)


# install.packages(c("lavaan", "semTools", "semPlot"))   # if needed
library(lavaan)
library(semTools)
library(semPlot)


########
# simulate data
# structure based on Chapman, Love, Staton, & Lahav (in review):

# define the hierarchical structural model
piesModel <- " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES =~ General + Feature + Image "


# specify population model
piesDataModel <- " General =~ 0.9*i1 + 0.7*i2 + 0.5*i3
                   Feature =~ 0.3*i3 + 0.7*i4 + 0.9*i5 + 0.5*i6  + 0.9*i7
                   Image   =~ 0.2*i3 + 0.8*i8 + 0.9*i9 + 0.8*i10 + 0.7*i11 
                   PIES =~ 0.7*General + 0.8*Feature + 0.8*Image"

# generate simulated data
set.seed(10001)    # another island Zip code
piesSimData.norm <- simulateData(piesDataModel, sample.nobs=3600)
print(head(piesSimData.norm), digits=2)

# convert the real numbers to 1-7 scale faux-Likert items
piesSimData <- data.frame(lapply(piesSimData.norm, 
                          function(x) { cut(x, breaks=7, labels=FALSE) } ))

### check the simulated data
library(car)
some(piesSimData)
library(psych)
describe(piesSimData)

# visually examine the relationships
library(car)
library(RColorBrewer)
scatterplotMatrix(piesSimData[, c(1, 2, 4, 5, 8, 9)], diag="histogram",
                  col=brewer.pal(3, "Paired"), ellipse=TRUE )

# check basic EFA structure (reality check)
factanal(piesSimData, factors=3)

######## END of data simulation



###
### Now do the SEM
###
### Hierarchical model with lavaan package

# fit the structural model to the simulated data
library(lavaan)
# define the hierarchical structural model [same as above; repeated here]
piesModel <- " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES =~ General + Feature + Image "

pies.fit <- cfa(piesModel, data=piesSimData)
summary(pies.fit, fit.measures=TRUE)


# plot it
library(semPlot)
semPaths(pies.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75)




# Comparison model -- always compare SEM to something!
# a 1-factor version: single PIES factor for all items
piesModelNH1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6  + 
                          i7 + i8 + i9 + i10 + i11 "

# fit it
pies.fit.NH1 <- cfa(piesModelNH1, data=piesSimData)


# a 3-factor, non-Hierarchical model. Remove PIES upper level factor,
# and assert that factors are only slightly correlated
#
# this model is nested within the general PIES model, as it asserts three
# factors but with correlation among them fixed to 0.1 (nuisance covariance)
piesModelNH3 <- " General =~ i1 + i2 + i3
                  Feature =~ i4 + i5 + i6  + i7
                  Image   =~ i8 + i9 + i10 + i11
                  General ~~ 0.1*Feature
                  General ~~ 0.1*Image
                  Feature ~~ 0.1*Image "

pies.fit.NH3 <- cfa(piesModelNH3, data=piesSimData)

# compare models
# install.packages("semTools")
library(semTools)
compareFit(pies.fit.NH1, pies.fit.NH3, pies.fit)



########
######## THE FOLLOWING PLOTS APPEAR IN THE BOOK, BUT THIS CODE DOES NOT
######## The code creates the illustrations but is not described in itself

library(semTools)
library(semPlot)
semPaths(pies.fit, what="mod", fade=FALSE, residuals=FALSE, 
         structural=TRUE, fixedStyle=c("black",1), freeStyle=c("black",1))

# with items
semPaths(pies.fit, what="mod", fade=FALSE, residuals=FALSE, 
         structural=FALSE, fixedStyle=c("black",1), freeStyle=c("black",1))

### plot the alternative structural models
# 1-factor version
semPaths(pies.fit.NH1, what="mod", fade=FALSE, residuals=FALSE, 
         structural=FALSE, fixedStyle=c("black",1), freeStyle=c("black",1) )
# 3-factor version
semPaths(pies.fit.NH3, what="mod", fade=FALSE, residuals=FALSE, 
         structural=FALSE, fixedStyle=c("black",1), freeStyle=c("black",1) )

######## END OF EXAMPLES
######## 



##### path analysis (general structural equation model)
# model from Iacobucci, 2009, J. Consumer Psychology, p. 677
#
# first define the theoretical model
# specifying Quality =~ 0 * Cost because of odd results otherwise;
# this matches Iacobucci's CFA results (p. 676) where Quality~Price = -0.03.
# Also we want to take them as separate, orthogonal influences here
satModel <- " Quality =~ CSat + Value + q1 + q2 + q3  + 0*Cost
              Cost    =~ Value + Repeat + c1 + c2 + c3
              Value   =~ CSat + v1 + v2 + v3
              CSat    =~ Repeat + cs1 + cs2 + cs3
              Repeat  =~ r1 + r2 + r3 "

### load the data if you prefer
satSimData <- read.csv("http://goo.gl/MhghRq")
summary(satSimData)

### alternatively ...
# now define the data generation model for simulation
# latent construct loadings similar to those in Iacobucci, p. 677
# item loadings assumed to be 0.9 per comment on that p. 677 that "loadings
# were all significant and large (0.90 and higher)"
satDataModel <- " Quality =~  0.59*CSat + 0.56*Value + 
                              0.9*q1 + 0.9*q2 + 0.9*q3 + 0*Cost
                  Cost    =~ -0.5*Value + -0.29*Repeat + 
                              0.9*c1 + 0.9*c2 + 0.9*c3
                  Value   =~  0.06*CSat + 0.9*v1 + 0.9*v2 + 0.9*v3
                  CSat    =~  0.48*Repeat + 0.9*cs1 + 0.9*cs2 + 0.9*cs3
                  Repeat  =~  0.9*r1 + 0.9*r2 + 0.9*r3 "

# simulate some data. Using N=200, although Iacobucci used 100 
#    (cf. p. 674). For SEM with highly correlated measures, N=100 is not many
set.seed(33706)  # continuing the island tour
satData.norm <- simulateData(satDataModel, sample.nobs=200)
satSimData <- data.frame(lapply(satData.norm, 
                          function(x) { as.numeric(cut(x, breaks=7)) } ))


######
###### these checks omitted from the book
# check it
library(car)
some(satSimData)
library(psych)
describe(satSimData)
# check item-item relationships
cor(satSimData)
# visually examine the relationships
library(RColorBrewer)
scatterplotMatrix(satSimData[, c(1, 2, 4, 5, 7, 8, 10, 11, 13, 14)], 
                  col=brewer.pal(3, "Spectral"), ellipse=TRUE )

# basic psychometrics
alpha(satSimData)
######  end of omitted section
######


# fit the model
# NB: using standardized latent variables because we don't care about the 
# manifest item scale, only the relative influence of the latent variables

sat.fit <- sem(satModel, data= satSimData, std.lv=TRUE)
summary(sat.fit, fit.measures=TRUE)

# and plot it

# book version
semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7, edge.label.cex=1)



# now define a simpler test model
# let each latent only influence one other latent
satAltModel <- " Quality =~ CSat  + q1 + q2 + q3 + 0*Cost
                 Cost    =~ Value + c1 + c2 + c3
                 Value   =~ CSat  + v1 + v2 + v3
                 CSat    =~ Repeat + cs1 + cs2 + cs3
                 Repeat  =~ r1 + r2 + r3 "

satAlt.fit <- sem(satAltModel, data=satSimData, std.lv=TRUE)


################
# plot it --- ONLY FOR PRINT IN BOOK, NOT EXPLAINED OR SHOWN AS CODE
semPaths(satAlt.fit, what="mod", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7,
         fixedStyle=c("white",1))     # using this to omit the path fixed to 0
################


# compare the 2 models
compareFit(sat.fit, satAlt.fit, nested=TRUE)


##############
# plot it --- ONLY FOR PRINT IN BOOK, NOT EXPLAINED OR SHOWN AS CODE
semPaths(sat.fit, what="mod", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7,
         fixedStyle=c("white",1))     # using this to omit the path fixed to 0
##############



# Partial Least Squares

#### semPLS 

# create a smaller data set, N=50 instead of 200 by sampling from satSimData
set.seed(90704)
satSimData2 <- satSimData[sample(nrow(satSimData), 50), ]
describe(satSimData2)

# try to fit CB-SEM to it ... it fails
sat.fit2 <- sem(satModel, data= satSimData2, std.lv=TRUE)

# and we see extreme values of coefficients
summary(sat.fit2, fit.measures=TRUE)


# build a model in semPLS to match previous sem "satModel"
# satModel <- " Quality =~ CSat + Value + q1 + q2 + q3  + 0*Cost
#               Cost    =~ Value + Repeat + c1 + c2 + c3
#               Value   =~ CSat + v1 + v2 + v3
#               CSat    =~ Repeat + cs1 + cs2 + cs3
#               Repeat  =~ r1 + r2 + r3 "


# the measurement model (manifest variables)
# in "from, to" format: Col1 == "arrow from", Col2 == "arrow to"
satPLSmm <- matrix(c(
  "Quality", "q1",
  "Quality", "q2",
  "Quality", "q3",
  "Cost",    "c1",
  "Cost",    "c2",
  "Cost",    "c3",
  "Value",   "v1",
  "Value",   "v2",
  "Value",   "v3",
  "CSat",    "cs1",
  "CSat",    "cs2",
  "CSat",    "cs3",
  "Repeat",  "r1",
  "Repeat",  "r2",
  "Repeat",  "r3" ), ncol=2, byrow=TRUE)


# specify the structural model (latent variable relationships)
# in "from, to" format: Col1 == "arrow from", Col2 == "arrow to"
satPLSsm <- matrix(c(
  "Quality", "CSat",
  "Quality", "Value",
  "Cost",    "Value",
  "Cost",    "Repeat",
  "Value",   "CSat",
  "CSat",    "Repeat" ), ncol=2, byrow=TRUE)


# install.packages("semPLS")
library(semPLS)

# build the model object
# use the data we simulated above for lavaan example #2
satPLS.mod <- plsm(data=satSimData2, strucmod=satPLSsm, measuremod=satPLSmm)

# fit the PLS model
satPLS.fit <- sempls(model=satPLS.mod, data=satSimData2)

# the manifest variable loadings (measurement model)
plsLoadings(satPLS.fit)

# examine the structural coefficients (structural model)
pathCoeff(satPLS.fit)


# plot the structural paths & coefficients
# ==> requires graphviz (www.graphviz.org) <==

if (FALSE) {            # following ALWAYS writes a file; run if desired
  pathDiagram(satPLS.fit, file = "satPLSstruc", full = FALSE, digits = 2,
    edge.labels = "values", output.type = "graphics", graphics.fmt = "pdf")
}


# R-squared for the latent variables
rSquared(satPLS.fit)


### Establishing model goodness is tricky for PLS
### the main goal of PLS is not to *fit* data, but to estimate paths *from* data

### Difference in model "fitness" is unclear, but
### bootstrap at least helps to estimate model's coefficient stability

# bootstrap PLS (for N=50 data)
# running 500 iterations which may a minute or two to run
#
# ==> UPDATE: in some versions of semPLS, the bootstrap stops due
#     to too many failures. The code here matches the book, 
#     but may need updates or future semPLS options to run
#
set.seed(04460)
satPLS.boot <- bootsempls(satPLS.fit, nboot=500, start="ones")
summary(satPLS.boot, type = "bca", level = 0.9)

# plot the bootstrap path coefficients
# uses lattice package for plotting (in base R by default)
# plotting just the structural parts (coef name matches "beta")
# ... adding friendly names from the model object
# ... adding vertical reference lines at -0.3, 0, 0.3
# ... and alpha for just a bit of color blending

parallelplot(satPLS.boot, reflinesAt = 0, alpha=0.8,
  varnames=attr(satPLS.boot$t, "path")[16:21],
  main="Path coefficients in 500 PLS bootstrap iterations (N=50)")

###


# now what if we use the FULL N=200 dataset (instead of N=50)?
satPLS.modF <- plsm(data=satSimData, strucmod=satPLSsm, measuremod=satPLSmm)
satPLS.fitF <- sempls(model=satPLS.mod, data=satSimData)

# check its coefficients and fit
pathCoeff(satPLS.fitF)

# bootstrap PLS
set.seed(04460)
satPLS.bootF <- bootsempls(satPLS.fitF, nboot=500, start="ones")
# summary(satPLS.bootF, type = "bca", level = 0.9)

parallelplot(satPLS.bootF, reflinesAt = 0, alpha=0.8,
  varnames=attr(satPLS.bootF$t, "path")[16:21],
  main="Path coefficients in 500 PLS bootstrap iterations (N=200)")

