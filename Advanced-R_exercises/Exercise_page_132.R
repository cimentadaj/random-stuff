# 1.

where2 <- function(fun, env = parent.frame()) {

    all_funs <- vector("character")
  
    # search through the objects inside each environment and bring the name of environment where fun was matched
    for (i in search()) {
      if (fun %in% ls(i, all.names = T)) {
        all_funs <- c(all_funs, i)
      }
    }
  
  if (length(all_funs) == 0) stop(fun, " not found")
  else all_funs
}


where2("median")

# 2.

get2 <- function(x, env = parent.frame()) {
  stopifnot(is.character(x))
  # check in which environment the object exists
  if (exists(x, envir = env, inherits = F)) {
    
  env[[x]]
  
  } else {
    
    get2(x, parent.env(env))
  
  }
}

get2("xtabs")

# 3.

fget <- function(x, env = parent.frame(), inherits = F) {
  if (identical(env, emptyenv())) {
    
    stop(fun, "is either not a function or was not found")
    
  } else if (exists(x, envir = env, inherits) && is.function(env[[x]])) {
    
    env[[x]]
    
  } else {
    
    fget(x, parent.env(env))
    
  }

}

fget("lm")

# 4. 
exists2 <- function(x, env = parent.frame(), inherits = F) {
   if(!inherits) return(x %in% ls(env, all.names = T))

  if (identical(env, emptyenv())) {
    FALSE
  } else if (x %in% ls(env, all.names = T)) {
    TRUE
  } else {
    exists2(x, parent.env(env), inherits = T)
  }
}

exists("print")
# If inherits is true, it only means that it will search for the object in parent frames and not only
# on the one specified.
