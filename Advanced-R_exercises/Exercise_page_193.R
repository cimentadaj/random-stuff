# 1.
x <- runif(1000)
summ <- function(summary_functions) {
        function(x) lapply(summary_functions, function(f) f(x, na.rm = T))
}

summ1 <- summ(list('min' = min, 'median' = median, 'mean' = mean, 'max' = max, 'quantile' = quantile))
summ1(x)

# 2.
# It depends.

# If function f and vector z are both inside list x, then x$f(x$z) is the correct answer. If only z is inside list x, then f
# is simply a function operating on z. The correct answer would be f(x$z)

# First example:
x <- list()
x$f <- identity
x$z <- 2

with(x, f(z))  # 2
x$f(x$z)       # 2

# Second example
x <- list()
x$z <- mtcars$mpg
with(x, identity(z))
identity(x$z)
