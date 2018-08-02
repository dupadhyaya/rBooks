# arrays
c <- 1:6
dim(c) <- c(3, 2)
c
class(c)
str(c)


#------
# One vector argument to describe all dimensions
b <- array(1:12, c(2, 3, 2))
b

length(b)
dim(b)
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
b
