# 1.
body(t); methods(t)
# The body of the function uses the function `UseMethods()` to dispatch
# to another method. This function is a generic function. In addition
# you can see that the methods function returns methods associated with the generic

# This doesn't happen with functions which are not generic
methods(t.default)

# What happens if you create an object with class t and use it with t()?
x <- structure(5, class = 'test') # test class object
t(x) # runs t.test() which is not what is supposed to!

methods(t)
# t dispatches to t.test() which is not transpose but t.test()!

# Alternatively
pryr::ftype(t.test)
# > pryr::ftype(t.test)
# [1] "s3"      "generic"
# tells us directly its an s3 generic rather than method, otherwise `pryr::ftype` would explicitly say "s3", "method"



# 2.
methods(Math)
# nonStructure-method
# structure-method
# data.frame
# Date
# difftime
# factor
# POSIXt

# How do the methods work?
# A part from making specific-class transformations, it always redirects to the .Generic function.
# It looks like the .Generic function is the one doing all the work, and is the main function behind everything.
# The distinct methods are just making sure the object is suitable for the .Generic function.

# 3.
posixlt <- grep(".POSIXlt$", ls("package:base"), value = TRUE)
posixct <- grep(".POSIXct$", ls("package:base"), value = TRUE)

index_find <-
  match(gsub(".POSIXlt", "", posixlt), gsub(".POSIXct", "", posixct), 0)

common_posixct <- posixct[index_find]
common_posixlt <- posixlt[index_find > 0]

x <- mget(common_posixct, baseenv())
y <- mget(common_posixlt, baseenv())

matched_funs <- sapply(seq_along(x), function(i) {
  # Check length of the bodies are the same
  len_true <- length(as.character(body(x[[i]]))) == length(as.character(body(y[[i]])))
  
  # If both length of bodies are the same, then check equality between bodies
  len_true && all(as.character(body(x[[i]])) == as.character(body(y[[i]])))
})

# Only print is the same!
x[matched_funs]


# 4.
library(pryr)

is.generic <- function(x) {
  is.element("generic", ftype(x))
}

objs <- mget(ls("package:base"), inherits = TRUE) # obtain all functions
base.funs <- Filter(is.function, objs) # Filter which ones are functions
base.generics <- Filter(is.generic, base.funs) # obtain which ones are generic

num.methods <- sapply(names(base.generics),function(x) length(methods(x))) # length of each one
num.methods[which.max(num.methods)] # which one is the higher

  # or
                      
pkgs <- ls("package:base")

is_funs <- sapply(pkgs, function(x) is.function(get(x, envir = baseenv())) && pryr::is_s3_generic(x))
all_methods <- sapply(pkgs[is_funs], function(x) length(as.character(methods(x))))

all_methods[which.max(all_methods)]

                      
                      
# | is the functions with the highest number of methods

# 5.
y <- 1
g <- function(x) {
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y # g.numeric is now the method g for the numeric class of g.
g(10) # This will give a 2, because g will dispatch for g.numeric, but x wasn't evaluated, so g.numeric searches
      # for the argument in the global environment.

h <- function(x) {
  x <- 10
  UseMethod("h")
}
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num", x)

h("a") # The result is char a
# h will determine the class of x, which is character, dispatches to h.character(), but x already got evaluated.
# h.character than pastes char with x, which is the x from the function call.

# The rules seem to be this: When the argument is evaluated, the method will use the argument from the function call
# but when the argument is lazily evaluated, it will look for the vector inside the generic and then outside.
# When x is evaluated in the function call, `UseMethod`'s black magic passes that x along to the methods.


# 6.
f <- function() 1
g <- function() 2
class(g) <- "function"

class(f)
class(g)

# But look at the difference here
attributes(f) # f does not have an attribute of class
attributes(g) # whereas g has

# this function is a method for generic length for the function class type
length.function <- function(x) "function"
length(f)
length(g) # when using length with the attribute class function, it calls the specific method

# The dispatchment is based on the attribute class.

# Hadley's point seems to be the last line of the the help page for ?"internal generic":
#"For efficiency, internal dispatch only occurs on objects, that is those for which is.object returns true."
# g is an object; f is not!:
