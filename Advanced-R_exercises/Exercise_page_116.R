# 1.
all_generics <- as.character(getGenerics())
all_classes <- as.character(getClasses())

get_num_methods <- function(fname, mode) {
  if (identical(mode, 'generic')) {
    n <- capture.output(showMethods(fname))
  } else if (identical(mode, 'class')) {
    n <- capture.output(showMethods(class = fname))
  } else {
    return(0)
  }
  length(n) - 2
}

number_methods <- sapply(all_generics, get_num_methods, 'generic')
number_methods[which.max(number_methods)]
# initialize 
#  47 

number_class_methods <- sapply(all_classes, get_num_methods, 'class')
number_class_methods[which.max(number_class_methods)]
# ANY 
# 160 

# 24/12/2017

# Get all functions in the global env
all_funs <- mget(unlist(sapply(search(), ls)), inherits = TRUE)

# Filter only those which are functions and S4
all_funss4 <- Filter(function(x) is.function(x) && isS4(x), all_funs)

# Get their methods and take the length
lengthy_s4 <- sapply(names(all_funss4), function(x) length(as.character(methods(x))))

# Subset the lengthiest one and that's coerce!
names(all_funss4)[which.max(lengthy_s4)]

# 2.

# From ?class
# Classes exist for which no actual objects can be created by a call to new,
# the virtual classes, in fact a very important programming tool.
# They are used to group together ordinary classes that want to share some
# programming behavior, without necessarily restricting how the behavior is
# implemented.

# Making a normal class

setClass("Nonvirtual", slots = list(name = "character"))
roland <- new("Nonvirtual", name = "Roland Freedom")
getClass('Nonvirtual')
isS4(roland)
#[1] TRUE

# Making a virtual class
setClass("Virtual", contains=NULL)
getClass('Virtual')

getClass('Virtual')@prototype

# You can't make objects for a virtual class
roland <- new("Virtual")

# But defining slots makes the object not virtual, even though contains=NULL
setClass("Virtual", contains=NULL, slots = list(name = 'character'))
getClass('Virtual')

roland <- new("Virtual")

# 3.

setClass("boss", slots = list(name = "character"))
roland <- new("boss", name = "Roland") # S4 object
# print is an S3 generic. 
"generic" %in% pryr::ftype(print)

# Let's pass it.

print(roland); roland # It gives the same output as the S4 object

# Let's try another, like as.character
"generic" %in% pryr::ftype(print)
as.character(roland)
# no method for coercing this S4 class to a vector
# So in order for a S3 generic to work on an S4 object it must have an appropriate method

### What happens if you pass an S3 object to an S4 generic?
setOldClass("Grandpa")

getClass("Grandpa")
# Virtual Class "Grandpa" [in ".GlobalEnv"]
#
#Slots:
#
#Name:   .S3Class
#Class: character
#
#Extends: "oldClass"

# Grandpa is S4 because setOldClass makes an S3 object as an S4 object
isS4(getClass("Grandpa"))
#[1] TRUE

setGeneric('some_method', function(x) {
  print(x)
})

isS4(some_method)
#[1] TRUE

some_method(getClass("Grandpa"))
# Virtual Class "Grandpa" [in ".GlobalEnv"]
#
#Slots:
#
#Name:   .S3Class
#Class: character
#
#Extends: "oldClass"
