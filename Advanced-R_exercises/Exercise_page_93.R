# 1.
infix <- mget(grep("<-",ls('package:base'), value = T), inherits = T)
m <- sapply(infix, is.primitive)
m[m == T]

# 2.
# Those which are surrounded by % %
# Like:
`%-%` <- function(a, b) a - b
    
    
# 3.
`%xor%` <- function(a, b) ifelse(a != b, TRUE, FALSE)
TRUE %xor% FALSE
TRUE %xor% TRUE
FALSE %xor% FALSE
TRUE %xor% TRUE

# 4.
# intersect function
`%int%` <- function(a, b) unique(a[match(a, b, 0) > 0])

# setdiff function
`%diff%` <- function(a, b) unique(a[match(a, b, 0) == 0])

# union function
`%union%` <- function(a, b) unique(c(a, b))

# 5.
`modif<-` <- function(x, value) {
    x[sample(length(x), 1)] <- value
    x
}

x <- rep(0, 5)
modif(x) <- 5; x
