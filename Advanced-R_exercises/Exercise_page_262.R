# 1
g <- function(x) deparse(substitute(x))

g(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z)

deparse_long <- function(expr, ...) {
  new_expr <- substitute(expr)
  deparse(new_expr, width.cutoff = 500, ...)
}

deparse_long(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z)

# 2
# as.Date.defalt()
# Uses it to capture the date as a string
# and use it in an error message

# Pairwise.t.test
# Uses it to capture the names of the variables
# and pastes it in the final output as information


# 3

pairwise.t.test(
  {p <- function() {
    x <- "this is a long message so that it has 60+ lines"; 
    rnorm(60)
  }
  p()
  }
  ,
  factor(sample(letters[1:3], 60, replace = TRUE))
  )

# 4

f <- function(x) substitute(x)
g <- function(x) deparse(f(x))

g(1:10)
g(x)
g(x + y ^ 2 / z + exp(a * sin(b)))

# This happens because f(x) in g
# actually captures x. If we called f() alone
# on the previous calls we would get what we expect. 
# It looks like substitute only works locally in it's own environment
# together with deparse
