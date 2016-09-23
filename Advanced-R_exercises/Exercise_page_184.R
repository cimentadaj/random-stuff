# 1.

finder <- function(funs, env = parent.frame()) {
  objects <- mget(ls(env), inherits = T)
  functions <- Filter(is.function, objects)
  ex <- vapply(functions, function(x) identical(funs, x), logical(1))
  
  if (any(ex)) {
    return(names(ex)[which(ex)])
  } else {
    finder(funs, parent.env(env))
  }
}

finder(xtabs)
finder(mean)
finder(sd)

# Functions are anonymous in R and are not bound to any particular name.
# There is also the problem that if the function has another name, it will find the first name that appears
# in the logical order of the environments.

# 2.

lapply(mtcars, function(col) sd(col)/mean(col))

# 3.

integrate(function(x) y = x ^ 2 - x, 0, 10)
integrate(function(x) y = sin(x) + cos(x), -pi, pi)
integrate(function(x) y = exp(x) / x, 10, 20)

# 4.

# at least in this exercise, I can't find a proper example because all of my anonymous functions are one liners
