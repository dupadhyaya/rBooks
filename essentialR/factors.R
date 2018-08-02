#Factors

x <- factor(c("a", "b", "b", "a"))
x
class(x)
levels(x)
# You ca't use values that are not in the levels
x[2] <- "c"
x

# NB: you can't combine factors
c(factor("a"), factor("b"))

# Factors are useful when you know the possible values a variable may take, even if you don’t see all values in a given dataset. Using a factor instead 
#of a character vector makes it obvious when some groups contain no observations:

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

table(sex_char)
table(sex_factor)


#Unfortunately, most data loading functions in R automatically convert character vectors to factors. 
#This is suboptimal, because there’s no way for those functions to know the set of all possible 
#levels or their optimal order. Instead, use the argument stringsAsFactors = FALSE to suppress this behaviour, and then manually convert character vectors to factors using your knowledge of the data. A global option, options(stringsAsFactors = FALSE), is available to control this behaviour, but I don’t recommend using it.

structure(1:5, comment = "my attribute")  # comment not seen

f1 <- factor(letters)
f1
levels(f1) <- rev(levels(f1))
f1

f2 <- rev(factor(letters))
f2
f3 <- factor(letters, levels = rev(letters))
f3
