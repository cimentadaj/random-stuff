#1

# If each call to FUN returns a vector of length n, then apply returns an array of dimension 
# c(n, dim(X)[MARGIN]) if n > 1.

apply(mtcars, 1, quantile, probs = c(0.25, 0.75))
# because th result of FUN is of length 2, it returns a matrix of 2 rows

# If n equals 1, apply returns a vector
# if MARGIN has length 1 and an
# array of dimension dim(X)[MARGIN] otherwise.

apply(mtcars, 1, mean)

# If n is 0, the result has length 0 but not necessarily
# the ‘correct’ dimension.

apply(mtcars, 1, function(x) numeric())

# If the calls to FUN return vectors of different lengths, apply returns a list of length prod(dim(X)[MARGIN])
# with dim set to MARGIN if this has length greater than one.

apply(mtcars, 2, unique)

#2

split_vapply <- function(num, fac, FUN, FUN.VALUE, ...) {
  split_df <- split(num, fac)
  vapply(split_df, FUN = FUN, FUN.VALUE = FUN.VALUE)
}

split_vapply(mtcars$disp, mtcars$cyl, mean, numeric(1))

# split + vapply makes sense partially.
# the whole point of tapply (split + lapply) is getting summary by groups.
# for example, a table for different groups. This leads to different lengths
# inevitably. But for other operations like group means, split + vapply makes sense

#3 
 
my_split <- function(num, f, drop = FALSE) {
  uni_f <- as.character(sort(unique(f)))
  splitted <- lapply(setNames(uni_f, uni_f), function(ind) num[f == ind])
  
  # drop null factors
  if (drop) splitted <- Filter(Negate(is.null), splitted)
  
  splitted
}
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))

 all(Map(function(x, y) all(x == y), my_split(x, g), split(x, g)))
