# 1.

vapply(mtcars, sd, numeric(1))

num <- vapply(iris, is.numeric, logical(1))
vapply(iris[num], sd, numeric(1))

# 2
# Because certain objects might have two classes and sapply returns a vector. In a case
# like this one, you will obtain a list instead of a a vector.

df2 <- data.frame(x = 1:10, y = Sys.time() + 1:10)
sapply(df2, class)

# Within a function, this might be problematic because you will be expecting a vector.
vapply(df2, class, character(1))

# More usefully, you get an error pointing to which element has length > 1

# 3.
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

sapply(trials, function(model) model$p.value)
sapply(trials, `[[`, 3)

# 4.
# replicate() repeats an expression N number of times.
# It does so by applying an sapply function n times over the expression provided
# the arguments naturally vary because it only takes the expression and the number
# of times it is repeated.

# In fact, we can replicate `replicate` doing this:
p <- sapply(1:100, function(i) t.test(rpois(10, 10), rpois(10, 10)), simplify = F)

# 5.
# I'm not sure I understand this question entirely but here's an approach seen from here:
# https://github.com/peterhurford/adv-r-book-solutions/blob/master/09_functionals/02_friends_of_lapply/exercise5.r
            
lapply2 <- function(X, names, FUN, ...) {
  stopifnot(length(X) == length(names))
  out <- vector("list", length(X))
  
  for (i in seq_along(X)) {
    out[[i]] <- FUN(X[[i]], ...)
  }
  
  names(out) <- names
  out
}

lapply2(list(1:10, 20:20), c("hey", "ho"),mean)            

# 6.

map_vapply <- function(..., FUN, FUN.VALUE) {
  
  # Save all inputs
  vec_list <- list(...)
  
  # check all inputs are the same length
  all_len <- vapply(vec_list, length, numeric(1))
  stopifnot(all(all_len[1] == all_len))
  
  # save the class and length of what the output should be
  the_class <- mode(FUN.VALUE)
  the_length <- length(FUN.VALUE)
  
  # create the list that will contain objects to parallel over. this is
  # the same length as the all the arguments in the function
  out <- vector("list", all_len[1])
  
  # This list is temporary that and will store
  # N arguments at a time. So supposing there are 3 vectors to 
  # parallel over, this list will have the first three elements of the
  # 3 vectors in the first run, and then the second elements of the
  # 3 vectors and so on.
  empty_out <- vector("list", length(vec_list))
  
  # if there are argument names, set them to the empty list
  if (!is.null(names(vec_list))) names(empty_out) <-  names(vec_list)
  
  # look through the empty out list (of length of
  # any of the ... in the fun; only one!)
  for (every_element in seq_along(out)) {
    
    # and then loop through the number of arguments in ...
    for (every_list in seq_along(vec_list)) {
      
      # collect the 1st elements on all the arguments in ...
      # collect the 2nd elements on all the arguments in ...
      empty_out[[every_list]] <- vec_list[[every_list]][[every_element]]
    }
    # store the first elements in the first slot of out
    # etc
    out[[every_element]] <- empty_out
  }
  
  # out is now a list of the length of any of the arguments in ...
  # the first slot contains the first element of all arguments in ...
  # the second slot contains the second element of all arguments in ...

  # loop through each of the arguments and run the FUN with do.call
  # and place the arguments as a list. Check the result is both the same
  # class and length as specified
  real_out <- lapply(out, function(args_list)  {
    fresult <- do.call(as.character(quote(FUN)), args_list)
    
    stopifnot(mode(fresult) == the_class)
    stopifnot(length(fresult) == the_length)
    fresult
  })
  # return final result
real_out
}

# will dispatch arguments in that some order to rnormr
map_vapply(100, 5, 2, FUN = rnorm, FUN.VALUE = numeric(100))

# named args will be matched and non-named will be put in order
# in the arguments of the FUN
map_vapply(100, n = 5, 2, FUN = rnorm, FUN.VALUE = numeric(5))

# for example, here the only remaining argument is mean and both
# first two args are named so the mean is set to 2.
map_vapply(sd = 100, n = 5, 2, FUN = rnorm, FUN.VALUE = numeric(5))

# can use anonymous function
map_vapply(rnorm(1e03), rnorm(1e03), rnorm(1e03), FUN = function(x, y, z) x + y + z, FUN.VALUE = numeric(1))
