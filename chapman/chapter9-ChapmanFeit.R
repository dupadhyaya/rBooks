###################################
# Code for: R for Marketing Research and Analytics, Chapter 9
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
# This file contains scripts used in Chapter 9 of Chapman & Feit (2015),
#   "R for Marketing Research and Analytics", Springer. 
#################################################################



######
###### Collinearity
###### 

cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

#### lm to predict online spend
spend.m1 <- lm(online.spend ~ ., 
               data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1)

library(gpairs)
gpairs(cust.df)


### Automatic data transformation

### autotransform function
autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )

summary(cust.df.bc)   # output not shown in the book
gpairs(cust.df.bc)    # output not shown in the book


#### lm to predict online spend, after transform
spend.m2 <- lm(online.spend ~ ., data=cust.df.bc)
summary(spend.m2)

# which is almost identical to a bivariate solution
spend.m3 <- lm(online.spend ~ online.trans, data=cust.df.bc)
anova(spend.m3, spend.m2)

# the problem
library(car)
vif(spend.m2)


# solution 1: omit covariates
#
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans, 
               data=cust.df.bc)

vif(spend.m4)
summary(spend.m4)


# solution 2: principal components
#
pc.online <- prcomp(cust.df.bc[ , c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[ , 1]

pc.store <- prcomp(cust.df.bc[ , c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[ , 1]

spend.m5 <- lm(online.spend ~ email + age + credit.score + 
                 distance.to.store + sat.service + sat.selection + 
                 online + store, 
               data=cust.df.bc)

summary(spend.m5)
vif(spend.m5)



######
###### Logistic Regression
######

exp(0) / (exp(0) + 1)  # computing logistic by hand; could use plogis()
plogis(-Inf)           # infinite dispreference = likelihood 0
plogis(2)              # moderate preference = 88% chance (e.g., of purchase)
plogis(-0.2)           # weak dispreference

log(0.5 / (1-0.5))     # indifference = 50% likelihood = 0 utility
log(0.88 / (1-0.88))   # moderate high likelihood
qlogis(0.88)           # equivalent to hand computation


### season pass data

# alternative code to load the data from website
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
summary(pass.df)

# 
# construct the data 

pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass") )
pass.tab


### detour into classes and attributes
###
class(c(1, pi, exp(1)))
class(data.frame(1:10))

str(pass.tab)

is.table(pass.tab)
is.character(pass.tab)

is.numeric(pass.tab)

as.numeric(pass.tab)
as.character(pass.tab)

names(pass.tab)
dim(pass.tab)
class(pass.tab)

dimnames(pass.tab)

attributes(pass.tab)
###
### END detour


### create data frame from table
### this is not strictly required but more closely mimics common data format

library(vcdExtra)   # install if needed
pass.df <- expand.dft(pass.tab)
str(pass.df)

table(pass.df$Pass, pass.df$Promo)

# factors are alphabetized ... but that would put "NoBundle" as higher level
# so let's reorder the factors to make "Bundle" higher
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
table(pass.df$Pass, pass.df$Promo)


###
### Logistic regression with glm()

# initial logistic regression model
pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

# how the coef translates to an odds ratio
plogis(0.3888)                          # outcome %
plogis(0.3888) / (1-plogis(0.3888))     # ratio of outcome % to alternative %
exp(0.3888)                             # identical


# odds ratio for sales
exp(coef(pass.m1))
# confidence intervals
exp(confint(pass.m1))


### at first it looks like the promotion is working
### but is this really the right model? check Channel
table(pass.df$Pass, pass.df$Channel)


# visualization
library(vcd)    # install if needed

doubledecker(table(pass.df))


# Model 2: add the effect of channel
pass.m2 <- glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)

# updated coefs and odds ratios
exp(coef(pass.m2))
exp(confint(pass.m2))

# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
summary(pass.m3)

# updated coefs and odds ratios
exp(confint(pass.m3))




##########################################################
####
#### ==> this section is NOT in the book
#### extras on visualization for logistic coefficients

# plot the coefs
library(coefplot)
coefplot(pass.m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         title="Coefficients for Season Pass by Factor", ylab="Factor")

#### plot the odds ratio confidence intervals
####
pass.ci <- data.frame(confint(pass.m2))     # coef confidence intervals
pass.ci$X50 <- coef(pass.m2)                # add the midpoint estimate

# plot odds
library(ggplot2)
pass.ci$Factor <- rownames(pass.ci)           # for ggplot2 to use in its model
pass.ci

# ggplot of odds ratios
# first: a plot by factor (x=) of the midpoint (y), high (ymax) and low (ymin)
p <- ggplot(pass.ci[-1, ], 
            aes(x=Factor, y=exp(X50), ymax=exp(X97.5..), ymin=exp(X2.5..)))

# ... displaying those elements as points & error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

# ... adding a vertical line at an odds ratio of 1.0 (no change)
p <- p + geom_hline(yintercept=1, linetype="dotted", size=1.5, color="red")

# now plot it with titles
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Card sign up odds by factor")) + coord_flip()


### exercise for reader ==> NOT in book
### does this add anything to our interpretation? Intercept model
pass.m3 <- glm(Pass ~ Promo * Channel, data=pass.df, family=binomial)
summary(pass.m3)
##########################################################


##### 
##### Hierarchical linear model
#####

### Ratings-based conjoint analysis data


########
# alternative to load the data
# ==> BUT as always we recommend to create it instead, as below
conjoint.df <- read.csv("http://goo.gl/G8knGV")
# OR:
# conjoint.df <- read.csv(paste("http://r-marketing.r-forge.r-project.org/",
#                               "data/rintro-chapter9conjoint.csv", sep=""))
conjoint.df$speed  <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
summary(conjoint.df)
########



###
### Construct the data
###
set.seed(12814)
resp.id <- 1:200 # respondent ids
nques <- 16 # number of conjoint ratings per respondent
speed <- sample(as.factor(c("40", "50", "60", "70")), size=nques, replace=TRUE)
height <- sample(as.factor(c("200", "300", "400")), size=nques, replace=TRUE)
const <- sample(as.factor(c("Wood", "Steel")), size= nques, replace=TRUE)
theme <- sample(as.factor(c("Dragon", "Eagle")), size=nques, replace=TRUE)

profiles.df <- data.frame(speed, height, const, theme)
profiles.model <- model.matrix(~ speed + height + const + theme, 
                               data=profiles.df)

library(MASS)     # a standard library in R
weights <- mvrnorm(length(resp.id), 
                   mu=c(-3, 0.5, 1, 3, 2, 1, 0, -0.5),
                   Sigma=diag(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3, 1, 1)))

# create df to hold the data
# better would be to preallocate; but for small data set, building up is OK
conjoint.df <- NULL   # make sure there's no data yet

# create per-respondent ratings
for (i in seq_along(resp.id)) {
  # create one respondent's ratings of the 16 items, plus error
  utility <- profiles.model %*% weights[i, ] + rnorm(nques)  # preference
  rating <- as.numeric(cut(utility, 10))   # put on a 10-point scale
  conjoint.resp <- cbind(resp.id=rep(i, nques), rating, profiles.df)
  # and add that respondent to the total data set
  conjoint.df <- rbind(conjoint.df, conjoint.resp)
}  

### END: constructing the data



### Hierarchical model
###
summary(conjoint.df)
by(conjoint.df$rating, conjoint.df$height, mean)


# basic linear model
ride.lm <- lm(rating ~ speed + height + const + theme, data=conjoint.df)
summary(ride.lm)

# hierarchical model
library(lme4)

# 0 = HLM with intercept only
# model with random intercept by respondent = (1 | resp.id)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id),  
                  data=conjoint.df)

summary(ride.hlm1)

fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

# model with random intercept & slope by respondent = (predictors | resp.id)
#
# consider 1M+ iterations; using 100K for somewhat faster time (~5 min)
# WARNING: slow, takes several minutes!
#
# note: set.seed not needed for lmer 
# (cf. https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015263.html)
ride.hlm2 <- lmer(rating ~ speed + height + const + theme + 
                   (speed + height + const + theme | resp.id),
                 data=conjoint.df,
                 control=lmerControl(optCtrl=list(maxfun=100000)))  # default = 10000

# population estimate
fixef(ride.hlm2)

# individual estimates, 1 row per respondent
# just the random (individual variation) part
head(ranef(ride.hlm2)$resp.id)
# the total effect for each respondent (fixed + random)
head(coef(ride.hlm2)$resp.id)

# the coefficients are the fixed effects + individual (random) effects
# demonstrating this for an arbitrary respondent (id #196)
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]


####
#### Hierarchical Bayes linear model, metric conjoint analysis
####

### LOAD conjoint.df DATA AS ABOVE

# standard lm with MCMC
library(MCMCpack)    # install if needed
set.seed(97439)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme, 
                        data=conjoint.df)
summary(ride.mc1)


# hierarchical lm with MCMC
# WARNING: SLOW! Takes approx. 3 minutes on 2014 Macbook Air
#
set.seed(97439)
ride.mc2 <- MCMChregress(fixed = rating ~ speed + height + const + theme, 
                         random = ~ speed + height + const + theme, 
                         group="resp.id", data=conjoint.df, r=8, R=diag(8))

str(ride.mc2)

#Try it!: dimnames(b2$mcmc)

# overall estimates
summary(ride.mc2$mcmc[ ,1:8])

# estimates for one respondent (respondent 196)
summary(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)])

# overall estimates ... again
summary(ride.mc2$mcmc[ ,1:8])

# estimates for wood construction
ride.constWood <- summary(ride.mc2$mcmc[ , grepl("b.constWood", 
                                                colnames(ride.mc2$mcmc))] 
                          + ride.mc2$mcmc[ , "beta.constWood"])

hist(ride.constWood$statistics[,1], 
     main="Preference for Wood vs. Steel", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4))

# 60 mph 
ride.speed60 <- summary(ride.mc2$mcmc[,grepl("b.speed60", 
                                            colnames(ride.mc2$mcmc))] 
                        + ride.mc2$mcmc[,"beta.speed60"])

hist(ride.speed60$statistics[,1], 
     main="Preference for 60mph vs. 40mph", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4))



summary(ride.mc2$mcmc[,c("beta.constWood", "VCV.constWood.constWood", 
                   "beta.speed60","VCV.speed60.speed60")])


#### Reflections on Model Comparison

# now that we have models from 2 models, we might compare the fixed effects
fix.hlm <- fixef(ride.hlm2)
fix.hb  <- colMeans(ride.mc2$mcmc[ , 1:8])

plot(fix.hlm, fix.hb)
abline(0,1)

# or compare random effects (in this case, for one respondent)
# in general, would want to compare full coefficients (fixed + random)
#
# but in this case, the fixed are nearly identical between the two,
# so we'll omit those for convenience
#
# LME random effects for ID #196
ranef(ride.hlm2)$resp.id[196, ]

# MCMC random effects for ID #196
colMeans(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), 
                                fixed=TRUE)])

# compare them graphically: 
# .. plot the distribution of the MCMC draws of the random effects for ID 196
# .. and then add distribution for the LME random effects for ID 196
# .. doing this for only the first 4 of the 7 non-intercept parameters


par(mfrow=c(2,2))       # make a 2x2 plot surface
plot.xlim <- c(-3, 3)   # define limits for the x-axis

for (i in 2:5) {        # first four parameters only, for convenience
  # plot the MCMC density for random effect i
  mcmc.col <- which(grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE))[i]
  plot(density(ride.mc2$mcmc[ , mcmc.col]), xlab="", 
       ylim=c(0, 1.4), xlim=plot.xlim,
       main=paste("HB & lmer density:",
                  colnames(ride.mc2$mcmc)[mcmc.col] ))
  # add the HLM density for random effect i
  hlm2.est <- ranef(ride.hlm2)$resp.id[196, i]               # mean estimate
  hlm2.sd <-  sqrt(attr(ranef(ride.hlm2, condVar=TRUE)$resp.id, 
                        "postVar")[ , , 196][i, i])
  seq.pts <- seq(from=plot.xlim[1], to=plot.xlim[2], length.out=1000) # range
  # .. find density at x-axis points using dnorm() and add that to the plot
  points(seq.pts, dnorm(seq.pts, mean=hlm2.est, sd=hlm2.sd), 
         col="red", pch=20, cex=0.4)                      
  legend("topright", legend=c("red = lmer", "black = HB"), 
         text.col=c("red", "black"))
}

