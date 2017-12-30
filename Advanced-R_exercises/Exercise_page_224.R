# 1

arg <- function(x, fun, max_min) {
  match.fun(fun)
  
  result <- fun(x)
  
  x[which(max_min(result) == result)]
}

arg(-10:5, function(x) x ^ 2, max)
arg(-5:5, function(x) x ^ 2, max)

arg(-10:5, function(x) x ^ 2, min)

# 2

# too much work haha
