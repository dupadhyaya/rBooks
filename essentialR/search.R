# Searching in Data Structure
data(Orange)
Orange
library("Zelig")
mydat <- gsource(
  variables = "
1 1 1 1
1 1 1 1
1 2 3 4
1 2 3 4
1 2 2 2
1 2 3 2")
unique(mydat) # keep unique rows
library(cwhmisc)
remove.dup.rows(mydat) # similar to unique()
table(duplicated(mydat)) # table duplicated lines
mydat$dups <- duplicated(mydat) # add a logical variable for duplicates