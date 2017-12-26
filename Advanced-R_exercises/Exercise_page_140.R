# 1.

# 1. A function has:
# - enclosing environment: which is where the function was defined
# - An execution environment: which is the environment where the exercution of the code will be carried out. This
# environment dissapears after the function ends.
# - The call environment: which is where functions are called to execute within the execution environment.
# - Binding environment: which is where functions are looked for, which is not necesarily the source package

# There is a distinctive difference between the binding and enclosing environment. The binding environment
# binds each function to its parent package, even though these functions are in other environments. So if your
# create a function named mean() and then you want to use the base::mean, you specify to look for it in the namespace
# which has every function binded to their enclosing environment, which is the package.

# 2.
f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)

# The enclosing environment of f1 is the global environment. The enclosing environment of f2 is f1.
# The enclosing environment of f3 is f2. When f3 is called, it will look for x1 and x2 in f2, it will find
# x2, and then it will continue searching for x1 in f1, and when it finds it, it prints the result.

# 26/12/2017

# The calls are executed this way: f1 is called with x1 = 1, f1 then defines f2 and calls f2 with x2 = 2
# f2 defines f3 and calls f3 with x3 = 3. f3 searches for x3 and finds it in the execution environment.
# Then looks for x2 and doesn't find it. Goes up to the parent environment and finds it. It then repeats
# for x1 and finds it in the grandparent environment.

# 3.

# 4.

# 5.

str(sd)

str2 <- function(fun) {

  funs <- list(funs = args(fun),
          call = environment(fun), 
          source = pryr::where(as.character(substitute(fun))))
  funs
}

str2(sd)
str2(print)
str2(summary)
