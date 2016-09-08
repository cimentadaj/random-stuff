# 1.
# is.function()
# is.primitive()

# 2.
objs <- mget(ls("package:base"), inherits = T)
funs <- Filter(is.function, objs) # applies the is function, to the objs list and subsets those
                                  # which are functions

funs_length <- sapply(funs, function(x) length(formals(x))) # number of args of each function
funs_length[which.max(funs_length)] # function with highest number of arguments

length(funs_length[funs_length == 0]) # Number of functions with 0 arguments
# I'm not sure what's special. That they all don't contain arguments but contain ...
# They're not all primitives, so it can't be that.

primitives <- Filter(is.primitive, objs)

# The three important components are the body, its environment and the arguments.

# When the function is a primitive or was created in the global environment
# it doesn't show its environment.

intersect # shows the environment
is.primitive(intersect)

sum # doesn't show the environment
is.primitive(sum)
