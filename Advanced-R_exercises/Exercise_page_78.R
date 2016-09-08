# 1.
# It will return a vector of length 1 with a 10 named c
c <- 10; c(c = c)

# 2.
# Name masking: R searches for values inside the environment of creation
# then searches for the environment above, and so on, until reaching the
# the global environment, when it starts searching for loaded packages.

# Functions vs names: Just as R searches for names, it does so with functions,
# inside its own environment and then the next one  and so on.
#  One difference is that when searching for a function, R ignores objects
#  which are not functions

# A fresh start: whenever a function is used it will create a new environment
# so any result that the function produced before doesn't exist. An example
# will show it better:

j <- function() {
    if (!exists("a")) { # asumming there's no object a
        a <- 1          # everytime you run this function you will obtain an a
    } else {
        a <- a + 1      # because everytime it will search for an a, a was never
    }                   # created OUTSIDE the function environment
    print(a)
}

j()
j()
# Dynamic Lookup: R searches for values at the time the function is ran.
# That's a problem because the result of a function might depend on the
# values currently available in the workspace. Make functions which are
# self-contained.


# 3.
# It will give 202 as an answer

f <- function(x) {
    f <- function(x) {
        f <- function(x) {
            x ^ 2 # looks for this x in it's own environment, then the next f function
        }         # and then the other f function
        f(x) + 1 # this f functions is searched inside its environment and finds an f
    }            # function inside(which should yield a 100) and adds 1
    f(x) * 2 # searches for the f function above with should yield 101 and mulitplies
}            # by 2

f(10) # exactly
