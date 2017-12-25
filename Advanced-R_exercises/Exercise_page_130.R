# 1.
# 1.1 To deleted an object you can't see it to null, e.g:
e <- new.env()
e$a <- 1:3
e$a <- NULL
ls(e)

# Instead, use rm
rm(a, envir = e)
ls(e)

# 2.1 names are unique within an environment and not in lists
list(a = 1:10, a = letters[1:10])

e$a <- 1:10
e$a <- letters[1:10]
ls(e); e$a

# 3.1 objects can be ordered inside an environment, as lists can.

# 2.
# ls() searches in the global environment
y <- "Hey, global envir!"
ls()

rm(y)
ls()

# Let's specify another environment
ls(e)

# What about <- ?
# ?`<-`
# The operators <- and = assign into the environment in which they are evaluated.
x <- 5 # is evaluated in the global environment
e$x <- 5 # is evaluted in the e environment

# 3.

new_env <- .GlobalEnv
nam <- environmentName(.GlobalEnv)

while (!identical(new_env, emptyenv())) {
  new_env <- parent.env(new_env)
  nam <- c(nam, environmentName(new_env))  
}
nam # These are all the parent frames of .GlobalEnv, with the exception of R_GlobalEnv

my_search <- function() {
  env <- .GlobalEnv
  env_show <- environmentName(env)
  
  while (!identical(env, emptyenv())) {
    env <- parent.env(env)
    env_show <- c(env_show, environmentName(env))
  }
  
  env_show
}

my_search()
