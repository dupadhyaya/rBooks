# Vector

int_var <- c(1L, 6L, 10L)
typeof(int_var)

is.integer(int_var)

is.atomic(int_var)

dbl_var <- c(1, 2.5, 4.5)
typeof(dbl_var)
is.double(dbl_var)

is.numeric(int_var)
is.numeric(dbl_var)

#Coercion
str(c("a", 1))

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)


s = c("aa", "bb", "cc", "dd", "ee")
L = c(FALSE, TRUE, FALSE, TRUE, FALSE) 
s[L] 
s[c(FALSE, TRUE, FALSE, TRUE, FALSE)] 
