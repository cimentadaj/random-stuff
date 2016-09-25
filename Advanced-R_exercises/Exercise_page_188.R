# 1.
# Because they enclose the environment of the parent function. Regardless of what you do
# with the anonymous function, it will always remember it's parent environment and search
# for values there.


# 2.

bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

# This function dispatches to two other different functions, depending on the value of lambda
# I can't think of a better name

lamb <- bc(0)
lamb2 <- bc(1)

lamb(10)
lamb2(10)

# 3.
approxfun()

# approxfun() actually prepares the data for another function. It makes sure the arguments have the correct
# lengths and classes. In case some arguments are not provided, it calculates some of its values.
# It returns a new function that accepts only one arguments. When you specify that argument, the new function
# will look for all other arguments in it's parent environment, i.e. approxfun()

# 4.
ecdf()

# It takes an argument and passes it to approxfun, then changes some of the properties of approxfun(classes and attributes)
# and returns the output of approxfun, which is a function. We could see ecdf() as a wrapper of approxfun
# for specific calculations.

# 5. 

moment <- function(i) {
  function(x) sum((x - mean(x)) ^ i) / length(x)
}

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

# 6.

pick <- function(i) function(x) x[[i]]

identical(lapply(mtcars, pick(5)), lapply(mtcars, function(x) x[[5]]))

# The pick function is called on mtcars. This function first assigns 5 as the i argument and the next
# function is returned. This function is now applied to each element of mtcars, with i being
# called from the parent environment.

# Test

picker <- pick(5)
lapply(mtcars, picker)

