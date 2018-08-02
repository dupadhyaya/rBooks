## Summarising Data

# addmargins -------
# summary on table, arrag, matrix. Specify margins - row or coln
?addmargins
addmargins(A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE)

# aggregate -----------
# computes statistics on complicated objects based on grouping variables.
aggregate( x, by, FUN, ...)
aggregate( formula, data, FUN, ..., subset, na.action = na.omit)
vec = 1:16  # 16 values
fac1 = gl(n=4, k=4, labels=LETTERS[1:4]) # Factor 4 levels " 4x4=16
fac2 = gl(n=2, k=8, labels=c('First', 'Second') ) # Factor 2 levels 2x8 =16
fac1; fac2
newdf = data.frame(resp = vec, pri=fac1, pr2 = fac2)
newdf  # 3 colns
# do aggregate functions on col1 based on other colns
aggregate(vec, by=list(fac1), FUN=max)
str(newdf)
aggregate(newdf$resp, by=list(newdf$pri), FUN=max)
aggregate(vec, by= list(fac1, fac2), FUN=median)
?table
ftable(newdf, pri)
