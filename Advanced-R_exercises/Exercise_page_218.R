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
