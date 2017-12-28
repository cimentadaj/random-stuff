# 1
# because a vector can't be of class NA.

is.character(letters)

# works because the whole vector is character.
# is.na() tests whether any of the elements within
# a vector is na
v <- c("a", NA, "BE")
is.na(v)

# the closest to is.na being a predicate is anyNA()
anyNA(v)

# 2
filter_vapply <- function(predicate, X, FUN, FUN.VALUE, ...) {
  vapply(Filter(predicate, X), FUN, FUN.VALUE, ...)
}

filter_vapply(is.numeric, iris, mean, numeric(1))

filter_vapply(is.numeric, iris, quantile, numeric(5))

# 3

# Position does two things:
# - it tests a predicte
# - returns the positions of TRUE
l <- list(num = rnorm(100), b = letters)

Position(is.numeric, l)

# which does the second step by it's own

# first step 
first <- vapply(l, is.numeric, logical(1))

# which returns positions of TRUE
which(first)

####

# where()
where <- function(x, f) {
  vapply(x, f, logical(1))
}

# on the other hand performs the first step, it tests a predicte
# and returns the logical showing which were true and others false.
# Filter() instead of returning what where() returns, subsets only theTRUES

# 4

Any <- function(l, predicate) {
  eval_l <- vapply(l, predicate, logical(1))
  
  sum(eval_l) >= 1
}

Any(l, is.numeric)
Any(l, is.data.frame)

All <- function(l, predicate) {
  eval_l <- vapply(l, predicate, logical(1))
  
  length(l) == sum(eval_l)
}

All(mtcars, is.numeric)
All(iris, is.numeric)

# 5

the_list <- list(
  rep(1:10, 1:10),
  rep(1:4, c(1, 2, 1, 4)),
  rep(12, 12),
  letters,
  iris$Species
)

span <- function(l, predicate) {
  # determine which elements to work on
  init <- vapply(l, predicate, logical(1))
  # save their indices
  which_true <- which(init)
  
  # take the sequences of each of elements that satisfy the
  # predicate
  sec <- lapply(l[init], rle)
  
  # grab the number of times the longest sequential number was repated
  # and detetermine the max of all.
  # this is to find the longest sequential number
  final <- which.max(sapply(sec, function(p) max(p$lengths)))
  
  # because we want to return the index of the original
  # list, we subset the final index from the original indexes
  which_true[final]
}


span(the_list, is.numeric)
