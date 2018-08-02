# Next Selecting and Sampling Data pg 107

#[]-------------
# select / extract parts of object
newlist = list(Ltrs=letters[1:5], Nmbrs=100:150)
newlist
newlist[2]
newlist[1]

newdf = data.frame(col1=1:3, col2 = 4:6)
newdf
newdf[1]
newdf[2:3,1:2]
newdf[1:2,]
newdf[,-2]
newdf[-3,]
newdf[-(1:2),]

newnum = c(1.5, 2.3, 4.7)
newnum[-2]  # all except 2 item

newarr = array(1:12, dim=c(2,3,2))
newarr
newarr[,c(1,3),2]
newarr[,c(2,3),1]

#$------------
newlm$coefficients
names(newlm)
names(newdf)
newdf$col1
newdf
newdf$col2 = c(10,11,12)
newdf$col2

#droplevels -----------
droplevels(x,except,...)
#drop unused levels
data("InsectSprays")
str(InsectSprays)
levels(InsectSprays$spray)
table(InsectSprays$spray)
ISs = subset(InsectSprays, spray != 'C')
head(ISs)
str(ISs)
levels(ISs$spray)
table(ISs$spray)
ISd = droplevels(ISs)
table(ISd$spray)


#resample ---------
#custom function to be created by user Pg 112
resample = function(x, ..,) x[sample(length(x), ...)]
resample(x, size, replace=FALSE)
newvec = 1:10
sample(newvec[newvec > 8])
sample(newvec[newvec > 9])  # this is wrong
sample(newvec[newvec > 10])
resample = function(x) x[sample(length(x))]  # create function
resample(newvec[newvec > 8])
resample(newvec[newvec > 9])  # this is ok now
resample(newvec[newvec > 10])

#sample --------------
#random samples & permutations
sample(x, size, replace=F)
newnum = 1:10
sample(newnum)  # n objects randomly
newchar = month.abb[1:12]
sample(newchar)  # replace =F
sample(newchar, replace=T)
sample(newchar, size=5)

set.seed(4)
sample(newchar,size=4)

sample(newnum,size=3)
set.seed(3)
sample(newnum[newnum > 5],size=2)

set.seed(3)
sample(newnum[newnum > 5])

set.seed(3)
sample(newnum > 5)  # index 6 9 7 10 9 

set.seed(3)
sample(newnum==5)

#subset -----------
#part of data from vectors, matrix, df based on conditions
subset(x, subset, select)
newdf = data.frame(val=1:12, fac=gl(n=4,k=3,labels=LETTERS[1:4]))
newdf
subset(newdf, subset= val > 5, select=c(val,fac))
subset(newdf, subset=(val > 5 & fac == 'C'), select=c(val,fac))
subset(newdf, subset=(fac == 'D'), select=c(val,fac))
subset(newdf, subset=(fac == 'D'), select=c(val))
subset(newdf, subset= fac %in% c('C','D'), select=c(val,fac))


#which -----------
#pg 116
#returns and index value for an expression
which(x, array.ind = FALSE)
newnum = 1:10
newchar = month.abb[1:12]
newarr = array(1:12, dim=c(2,3,2))  # 3D array
newarr
dimnames = list(letters[1:2], LETTERS[1:3], c("One","Two")) # dim of 3D array

newchar
which(newchar == 'Apr')
newchar[which(newchar == 'Apr')]

newnum
which(newnum==5)
which(newnum > 5)

newarr
which(newarr > 5)
which(newarr > 5, arr.ind = TRUE)  # ???
# indices where condition is satisfied.  2,3,1 - 

# with -------

rm(list=ls())
## Sorting and Rearranging Data -----------
# order ---------
order(..., na.last=TRUE, decreasing = FALSE)
newvec = c(3,4,NA, 7,1,6,5,5,2)
tv1 = 1:9
tv2 = 9:1
tv1 ;tv2
newvec
order(newvec)   # order of indices
order(newvec, na.last=TRUE)  # NA at end
order(newvec, na.last=FALSE)  # omit NA
order(newvec, decreasing=TRUE)
order(newvec, decreasing=FALSE)   # order of indices
order(newvec, tv1)  #?
order(newvec, tv2) #?

#rank ------------
rank(x, na.last = TRUE, ties.method = 'average')
newvec = c(3,4,NA, 7,1,6,5,5,2)
rank(newvec)
rank(newvec, na.last = NA)  # remove NA
rank(newvec, na.last = FALSE)  # NA in end
rank(newvec, na.last = TRUE)  # NA in begin
rank(newvec, na.last = FALSE, ties.method = 'max')
newvec

#sort -------------
sort(x, decreasing = FALSE, na.last = NA)
# rearrange data into a new order
newvec = c(3,4,NA, 7,1,6,5,5,2)
sort(newvec)  # NA omitted
sort(newvec, na.last=TRUE) # NA at last
sort(newvec, na.last=FALSE) # NA at first
sort(newvec, decreasing=TRUE)  # NA omitted & decreasing

