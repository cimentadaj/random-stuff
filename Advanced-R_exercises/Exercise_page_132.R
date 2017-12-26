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

# 25/12/2017

where <- function(name) {
  # Empty env list
  # First environment
  env_list <- vector("list")
  env <- parent.frame()
  
  # While we don't reach the empty environment, if the name exists
  # in that environment, attache the env to the list and change to the parent
  # of that environment.
  while (!identical(env, emptyenv())) {
    if (exists(name, where = env, inherits = FALSE)) env_list <- c(env_list, env)
    env <- parent.env(env)
  }
  
  env_list
}

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

# 25/12/2017

my_get <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
      
    stop("Object ", name, " is not in any environment", call. = FALSE)
      
  } else if (exists(name, where = env, inherits = FALSE)) {
      
    env_exist <- as.environment(env)
    env_exist[[name]]
      
  } else {
    my_get(name, parent.env(env))
  }
}


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

# 25/12/2017
# this one is more complicated because I construct
# inherits by recursively looking up instead of just passing
# inherits to exists, like in the function above.
fget <- function(name, env = parent.frame(), inherits = TRUE) {
  env_obj <- env[[name]]
  
  if (inherits) { # If inherits, then search within the env and recursively look up
      
    if (identical(env, emptyenv())) {
      stop("Object ", name, " is not in any environment", call. = FALSE)
    } else if (!is.null(env_obj) && is.function(env_obj)) {
      env_obj
    } else {
      fget(name, parent.env(env))
    }
      
  } else { # if not, then simply check in the envionrment and return error if not.
      
    if (!is.null(env_obj) && is.function(env_obj)) {
      env_list[[name]]
    } else {
    stop("Object ", name, "not here")
        
   }
 }
}


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

# 25/12/2017

my_exists <- function(name, env = parent.frame(), inherits = TRUE) {
  if (identical(env, emptyenv())) {
    FALSE
  } else if (match(name, ls(envir = env), 0) != 0) {
    TRUE
  } else {
    my_exists(name, parent.env(env))
  }
}

