# 1.

vapply(mtcars, sd, numeric(1))

num <- vapply(iris, is.numeric, logical(1))
vapply(iris[num], sd, numeric(1))

# 2
# Because certain objects might have two classes and sapply returns a vector. In a case
# like this one, you will obtain a list instead of a a vector.

df2 <- data.frame(x = 1:10, y = Sys.time() + 1:10)
sapply(df2, class)

# Within a function, this might be problematic because you will be expecting a vector.
vapply(df2, class, character(1))

# More usefully, you get an error pointing to which element has length > 1

# 3.
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

sapply(trials, function(model) model$p.value)
sapply(trials, `[[`, 3)

# 4.
# replicate() repeats an expression N number of times.
# It does so by applying an sapply function n times over the expression provided
# the arguments naturally vary because it only takes the expression and the number
# of times it is repeated.

# 5.

# 6.
