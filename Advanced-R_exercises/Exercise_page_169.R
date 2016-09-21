# 1.
col_means <- function(df) {
  numeric <- sapply(df, is.numeric)
  numeric_cols <- df[, numeric]

  data.frame(lapply(numeric_cols, mean))
}

col_means(mtcars)
col_means(mtcars[, 0]) # This is wrong because there won't be columns to calculate
col_means(mtcars[0, ]) # This is wrong because there won't be rows to calculate
col_means(mtcars[, "mpg", drop = F]) # Problem because the output will be a vector
col_means(1:10) # Not a data frame, subsetting will through an error
col_means(as.matrix(mtcars)) # Not a data.frame, sapply will give a wrong result for subsetting
col_means(as.list(mtcars)) # Problem with subsetting because it's a list

mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)

### Solution ###

col_means <- function(df) {
  stopifnot(is.data.frame(df), match(0, dim(df), nomatch = 0) == 0 )
  # added filters for is data frame and that the dimension is above 0
  numeric <- vapply(df, is.numeric, logical(1)) # added vapply instead of sapply
  numeric_cols <- df[, numeric, drop = F] # added drop = F
  data.frame(lapply(numeric_cols, mean))
}

col_means(mtcars)
col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg", drop = F])
col_means(1:10)
col_means(as.matrix(mtcars))
col_means(as.list(mtcars))

mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)

# 2.
lag <- function(x, n = 1L) {
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

### Solution ###
lag <- function(x, n = 1L) {
  if (!is.vector(n) || length(n) != 1) stop("x must be a vector of length > 1")
  if (n > length(x)) stop("n is longer than x")
  if (n == 0) warning("n is equal to 0, same vector returned")
    
    xlen <- length(x)
    c(rep(NA, n), x[seq_len(xlen - n)])
}

lag(seq(4), 0)
lag(seq(4), 5)
lag(seq(4), c(1,2))
