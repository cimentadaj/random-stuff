
# 1.
mtcars[mtcars$cyl == 4, ]
mtcars[-(1:4), ]
mtcars[mtcars$cyl <= 5, ]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]
# or
mtcars[mtcars$cyl %in% c(4,6), ]

# 2.
x <- 1:5; x[NA] # No idea. Maybe because you're subsetting an NA, and that's not possible?
                # Another thing would be to subset x[x == NA] in case there are NA's.

# 3.
x <- outer(1:5, 1:5, FUN = "*")

upper.tri(x) # will yield a logical vector of the upper triangel of a matrix
x[upper.tri(x)] # since x is a matrix, a logical subset will subset the interger and not the matrix
                # the result will be an interger and not a matrix. Subsetting like this and
                # obtaining a matrix shape object only works in data frames because its like
                # a list.

# 4. 
mtcars[1:20] # is subsetting 20 columns, which mtcars does not have.
mtcars[1:20, ] # is subsetting the first 20 rows, which mtcars does have

# 5.
mydiagonal <- function(x) {
         sapply(1:ncol(x), function(i) x[i,i])
}
all(mydiagonal(matrix(1:100, 50, 50)) == diag(matrix(1:100, 50, 50)))


# 6.
set.seed(12)
df <- data.frame(nums = sample(c(1:10,NA)), let = letters[1:11])
df[is.na(df)] # is.na(df) will test if there is any NA's in the data frame
              # however, this subsetting is subetting columns, thus the result is
              # NA.
is.na(df) # shows the 6th row is TRUE
df[is.na(df)] <- 0 # this will change anything that is TRUE to zero
