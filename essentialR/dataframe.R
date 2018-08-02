# Data Frames
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
df
str(df)

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE)
str(df)

cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))

#When combining column-wise, the number of rows must match, 
#but row names are ignored. When combining row-wise, both the number and 
#names of columns must match. Use plyr::rbind.fill() to combine data frames
#that don’t have the same columns.
#http://adv-r.had.co.nz/Data-structures.html

bad <- data.frame(cbind(a = 1:2, b = c("a", "b")))
str(bad)

good <- data.frame(a = 1:2, b = c("a", "b"),
                   stringsAsFactors = FALSE)
str(good)

# Special Colns
df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)
df
str(df)

dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
str(dfl)
#I() adds the AsIs class to its input, but this can usually be safely ignored.

dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))
str(dfm)

# Search
head(mtcars)
mtcars[mtcars$mpg>25.0,]
mtcars[rownames(mtcars) == 'Fiat 128',]
mtcars[mtcars$gear == 4,]
mtcars[rownames(mtcars) == '^M',]
mtcars[grep("^M", rownames(mtcars), ignore.case=T),]
mtcars[grep("^4", mtcars$gear), ]
subset(mtcars, carb == '2')
subset(mtcars, hp==110)
mtcars[9]
mtcars[[9]]
mtcars[['am']]
mtcars$am
mtcars[,'am']

str(airquality)
subset(airquality, Temp > 80, select = c(Ozone, Temp))
subset(airquality, Day == 1, select = -Temp)
subset(airquality, select = Ozone:Wind)

with(airquality, subset(Ozone, Temp > 80))

nm <- rownames(state.x77)
nm
subset(state.x77, grepl("^M", nm), Illiteracy:Murder)
head(state.x77)

sw <- swiss[1:5, 1:4]  # select a manageable subset
sw

sw[1:3]      # select columns
sw[, 1:3]    # same
sw[4:5, 1:3] # select rows and columns
sw[1]        # a one-column data frame
sw[, 1, drop = FALSE]  # the same
sw[, 1]      # a (unnamed) vector
sw[[1]]      # the same
mtcars[1,2]
sw[1,]       # a one-row data frame
sw[1,, drop = TRUE]  # a list

str(sw)
sw["C", ] # partially matches
sw[match("C", row.names(sw)), ] # no exact match
try(sw[, "Ferti"]) # column names must match exactly
try(sw[, "Fertility"]) # column names must match exactly

swiss[ c(1, 1:2), ]   # duplicate row, unique row names are created

sw
#(sw[sw <= 6] <- 6)  # logical matrix indexing
sw <=12
length(sw[sw <=12])
sw

## adding a column
sw["new1"] <- LETTERS[1:5]   # adds a character column
sw
sw[["new2"]] <- letters[1:5] # ditto
sw
sw[, "new3"] <- LETTERS[1:5] # ditto
sw
sw$new4 <- 1:5
sw
sapply(sw, class)

sw$new4 <- NULL              # delete the column
sw
sw[6:8] <- list(letters[10:14], NULL, aa = 1:5)
sw

# update col. 6, delete 7, append
sw

## matrices in a data frame
A <- data.frame(x = 1:3, y = I(matrix(4:6)), z = I(matrix(letters[1:9], 3, 3)))
A
A[1:3, "y"] # a matrix
A[1:3, "z"] # a matrix
A[, "y"]    # a matrix

## keeping special attributes: use a class with a
## "as.data.frame" and "[" method:


as.data.frame.avector <- as.data.frame.vector

`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}

d <- data.frame(i = 0:7, f = gl(2,4),
                u = structure(11:18, unit = "kg", class = "avector"))
str(d[2:4, -1]) # 'u' keeps its "unit"


# Sort DF
mtcars
str(mtcars)
sort(mtcars)
sort(mtcars[1,])
sort(mtcars[,1])
sort(mtcars$mpg)
?sort
order(mtcars$mpg)
mtcars[order(mtcars$mpg), ]
?order
order(mtcars$mpg, mtcars[,2])
with(mtcars, order(mpg, cyl))
rank(mtcars$mpg)
mtcars$mpg
mtcars[order(mtcars$mpg, mtcars[,2]), ]

library(dplyr)
dplyr::arrange(mtcars)
?arrange
arrange(mtcars, cyl, disp)
arrange(mtcars, desc(disp))

slice(mtcars,5:n())
slice(mtcars,1L)
slice(mtcars,1:n())
str(mtcars)
by_cyl <- group_by(mtcars, cyl)
slice(by_cyl, 1:2)
filter(mtcars, row_number() == 1L)
filter(mtcars, row_number() == n())
filter(mtcars, between(row_number(), 15, n()))
summarise(mtcars, mean(disp))
filter(mtcars, cyl < 6, vs == 1)
filter(mtcars, cyl < 6 & vs == 1)

str(iris)
?tbl_df
iris <- tbl_df(iris) # so it prints a little nicer
select(iris, starts_with("Petal"))
select(iris, ends_with("Width"))
select(iris, contains("etal"))
select(iris, matches(".t."))
select(iris, Petal.Length, Petal.Width)
vars <- c("Petal.Length", "Petal.Width")
select(iris, one_of(vars))
mutate(mtcars, displ_l = disp / 61.0237)
transmute(mtcars, displ_l = disp / 61.0237)

mutate(mtcars, cyl = NULL)