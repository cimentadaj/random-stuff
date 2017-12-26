# 1.
rebind <- function(name, value, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if (exists(name, envir = env, inherits = FALSE)) {
    assign(name, value, envir = env)
  } else {
    rebind(name, value, parent.env(env))
  }
}
rebind("a", 10)
#> Error: Can't find a
a <- 5
rebind("a", 10)
a
#> [1] 10

# 1. This function looks if a variable exists in the global environment and if it doesn't
# it looks for it in parent environments.
# The main difference with <<- is that if the variable does not exists, it does not create it.
# <<- replaces or creates the variables regardless if it exists.


# 2.
assign2 <- function(var, value, env = parent.frame()) {
  if (!exists(var, where = env, inherits = F)) {
    assign(var, value, envir = env)
  } else {
    stop(var, " already exists")
  }
}

assign2("x", 1:5)
assign2("x", 2)

# 26/12/2017

assign2 <- function(name, value, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name)
  } else if (exists(name, env = env, inherits = FALSE)) {
    env[[name]] <- value
  } else {
    assign2(name, value, parent.env(env))
  }
}

# 3.

binder <- function(var, value, env = parent.frame(), active = T, locked = F) {
  if (active) assign(var, value, envir = env)
  else delayedAssign(var, value, assign.env = env)
  
  if (locked) lockBinding(var, env = env)
}

binder("x", 5) # should create the variabe in the specified environment
binder("p", 5, locked = T); p <- 8 # variable should be locked
system.time(binder("l", {Sys.sleep(1); 1}, active = F)) # variable is a promise
system.time(l) # now it runs
