#data2
### Viewing Data
## Listing Data
#attach ------------
attach(what)
newdf = data.frame(col1 = 1:3, col2=4:6)  # 2 colns
newdf
newlist = list(item1 = letters[1:5], item2=100:150)
newlist
item1; col1
attach(newdf)
attach(newlist)
item1
item2
col2
search()
detach(newdf)
detach(newlist)

#detach ---------
detach(name)
detach(package:name)
library(MASS)
ls()
search()
detach(package:MASS)
search()

#dir and list.files -------
dir(path ='.', pattern=NULL, all.files = FALSE, ignore.case = FALSE)
list.files(path='.', pattern=NULL, all.files = FALSE, ignore.case = FALSE)
dir()
dir('..')
dir(all.files = TRUE)
list.files()
list.files(path='..')
# pattern ?

#getwd ----------
getwd()

#head -----------
head(x, n=6L)  # no of elements to view 6
head(DNase)
head(DNase, n=3)
newmat = matrix(1:100, nrow=20, dimnames = list(letters[1:20]))
newmat
head(newmat, n=4)
head(newmat, n=-18) # all except last 18
head(newmat, n=-2) # all except last 2

#ls ------
ls(name, pos=-1, pattern, all.names=FALSE)
objects(name, pos=-1, pattern, all.names= FALSE)
ls()
ls(pattern='new*')
ls(pattern="^n")
ls(pattern='^n')
ls(pattern='^n|^u')

#rm / remove ----
rm(.., list= character(0), pos=-1)
remove(.., list=character(0), pos=-1)
ls(pos=2)

#search -------------
search()

#setwd ---------------
getwd()
setwd("/duwork/rWork/projects/gardener/essentialR")

#tail -------
tail(x, n=6L)
tail(DNase)
tail(DNase, n=2)
tail(DNase, n=-174)

#View ----------
View(x)
newvec = month.abb[1:6]
newdf = data.frame(col=1:3, col2=4:6)
newdf
newlist = list(item1 = letters[1:5], item2=100:110)
newlist
newmat = matrix(1:12, nrow=4, dimnames=list(letters[1:4], LETTERS[1:3]))
newmat
View(newvec)
View(newmat)
View(newdf)
View(newlist) # cannot be viewed by this command

#with ----------
with(x, expr)
newdf = data.frame(col1=1:3, col2=4:6)
newdf
newlist = list(item1=letters[1:5], item2=100:110)
newlist
col1
with(newdf, col1)
with(newlist, summary(item2))
with(newdf, mean(col2, na.rm=TRUE))

### Data Object Properties
## 
# attr ----------
attr(x, which, exact=FALSE)
attr(x, which, exact=FALSE) <- value
newdf = data.frame(col1=1:3, col2=4:6)
newdf
attributes(newdf)
attr(newdf, which='names')
attr(newdf, which='row.names')
attr(newdf, which='row.names') <- c('First', 'Second', 'Third')
attr(newdf, which='comment') <- c("Data with ammended attributes")
attributes(newdf)

attr(newdf, which='comment') <- NULL
attributes(newdf)

obj = 1:12
attr(obj, which='dim') <- c(3,4)
obj
class(obj)
attributes(obj)
attr(obj, which='dim') <- c(6,2)
obj
attributes(obj)

#attributes -------
attributes(obj)


#case.names ----
# case names for fitted model or row names for DF and Matrix objects
case.names(object)
newmat = matrix(1:12, nrow=3, dimnames=list(letters[1:3], LETTERS[1:4]))
newdf = data.frame(col1=1:3, col2=4:6, row.names = letters[1:3])
newmat; newdf
newlm = lm(col2 ~ col1, data=newdf)
case.names(newmat)
case.names(newdf)
case.names(newlm)


#class------


#colnames ---
colnames(newmat)

newnames=c('First','Second','Third', 'Fourth')
colnames(newmat) = newnames
newmat

#comment
comment(newmat) = 'This is comment'
comment(newmat)
attributes(newmat)
class(newmat)
comment(newmat) =NULL

#dim
dim(newmat)
# vector retuns NULL
newlist = list(Ltrs=LETTERS[1:5],Nmbrs=1:5)
dim(newlist)
newmat = matrix(1:24,ncol=4)
dim(newmat)
newdf = data.frame(col1=1:3, col2=4:6)
dim(newdf)
newdf
newchar= month.abb[1:10]
dim(newchar)

#dimnames--------------pg84
newmat
newmat = matrix(1:12,ncol=4,dimnames =list(c('R1','R2','R3'), c('C1','C2','C3','C4')))
dim(newmat)
dimnames(newmat)
newmat

newdf = data.frame(col1=1:6, col2=7:12, row.names = c('R1','R2','R3','R4','R5','R6'))
dimnames(newdf)
dimnames(newdf)[[2]] = month.abb[10:11]
newdf

newmat = matrix(1:12, ncol=3)
dimnames(newmat)
dimnames(newmat) = list(letters[1:4],LETTERS[1:3])
dimnames(newmat)
newmat
dimnames(newmat)[[1]] = month.abb[1:4]
newmat

#length---------

#levels -------------
levels(x)
levels(x) = value
newfac = gl(n=3, k=3, length = 9)
newfac
levels(newfac)
levels(newfac) = c(4,5,6)
levels(newfac)
levels(newfac) = c('a','b','c')
levels(newfac)
newfac

#ls.str ---------
ls.str(pos=-1, name, all.names = FALSE, pattern)
ls(pattern='^new')
ls.str(pattern='^new')
ls()
search()
ls(pos=1)  # GlobalEnv

#lsf.str --------------
lsf.str()
manning = function(radius, gradient, coeff) {
  (radius^(2/3) * gradient^0.5 / coeff)
}

cubrt = function(x) { 
  x^(1/3)
}
lsf.str()

#mode ---------
# attribute related to its type
mode(x)
mode(x) = value
mode(newlist)
mode(newmat)
mode(newdf)

#names ----------
newdf = data.frame(col1=1:3, col2=4:6)
names(newdf)
newdf = data.frame(1:3, 4:6)
names(newdf)

newmat = matrix(1:12, ncols=3)
names(newmat)

newvec = 1:6
names(newvec) 

names(newdf) = c('One','Two')  
names(newdf)

names(newmat) = c('One','Two','Three')
names(newmat)  # does not work
# use colnames & dimnames for matrix
colnames(newmat) = c('One','Two','Three')
colnames(newmat)  # vector stored in col & rows

newlist = list(letters[1:5],100:110)
names(newlist)
names(newlist) = c('Letters', 'Numbers')
names(newlist)

#ncol -------
nrow(newdf) ; nrow(newmat); nrow(newvec) ; nrow(newlist)

NROW(newdf) ; NROW(newmat) ; NROW(newvec) ; NROW(newlist)

ncol(newdf) ; ncol(newmat) ;ncol(newvec) ; ncol(newlist)

NCOL(newdf) ; NCOL(newmat) ; NCOL(newvec) ; NCOL(newlist)

#nlevels ----------
nlevels(newdf)
newfac = gl(n=4, k=3)
nlevels(newfac)
newvec = c('First', 'Second', 'Third')
fac2 = factor(newvec)
nlevels(newvec)
nlevels(fac2)

#relevel ----------
#shift the level for reference
relevel(x, ref)
newfac = gl(n=4, k=3, labels=letters[1:4])
newfac
nlevels(newfac)
newfac= relevel(newfac, ref='c')
newfac
newfac= relevel(newfac, ref='d')
newfac

#reorder -------------
# reorder based on other values 
reorder(x, X, FUN=mean,...)
newfac = gl(n=4, k=4, labels=letters[1:4])
newfac
length(newfac)
newvec = c(1:4, 4:7, 6:9, 2:5)
newvec
length(newvec)
reorder(newfac, newvec, FUN=mean)
reorder(newfac, newvec, FUN=median)
newfac
newvec
reorder(newfac, newvec, FUN=sum)
newfac2 = reorder(newfac, newvec, FUN=sum)
newfac2

boxplot(newvec ~ newfac)
boxplot(newvec ~ reorder(newfac, newvec, FUN=median))

#....


#row.names --------Pg 99
#DF only
row.names(x)
row.names(x) = value

newdf = data.frame(col1=1:3, col2=4:6)
row.names(newdf)
row.names(newdf) = paste('R',1:3,sep='-')
row.names(newdf)

#rownames --------
# matrix and Df
rownames(newdf) = paste('RR',1:3,sep='%')
newdf
newmat = matrix(1:12, nrow=4)
rownames(newmat)
rownames(newmat) = paste('r',1:4,sep='')
newmat
rownames(newmat)

#str ----------
str(newdf)
str(newmat)
str(newvec)
str(newlist)

#typeof ---------
# same as storage.mode

#unclass -----
class(newdf)
unclass(newdf)
class(newdf)  # df as list
unclass(newmat)  # not affected
unclass(newlist)
class(unclass(newdf))

# unlist ---------
newlist
unlist(newlist, use.names = TRUE)
unlist(newlist, use.names = FALSE) # list to 1 vector

#variable.names -----------
# of fitted models or colnames of DF & matrix
variable.names(newdf)

variable.names(newmat)
newmat = matrix(1:12, nrow=3, dimnames=list(letters[1:3], LETTERS[1:4]))
variable.names(newmat)
newmat
newlm = lm(col2 ~ col1, data=newdf)
variable.names(newlm)

#page no 107
# Next Selecting and Sampling Data