#List
#lists are different from atomic vectors because their elements can be of any type, 
#including lists. You construct lists by using list() instead of c():

x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

# recursive
x <- list(list(list(list())))
str(x)
is.recursive(x)

x <- list(list(1, 2), c(3, 4))
y <- c(list(1, 2), c(3, 4))
str(x)
str(y)

is.list(mtcars)
mod <- lm(mpg ~ wt, data = mtcars)
is.list(mod)


l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l
#These are relatively esoteric data structures, but can be useful if you want to arrange objects into a grid-like structure. 
