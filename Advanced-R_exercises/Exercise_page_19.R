# 1.
is.interger(); is.numeric(); is.logical(); is.character(); is.complex(); is.raw()

# A list differs from a vector because it can store elements of different classes

# 2.
# is.vector() and is.numeric() tests if the an object is a vector and the type.
# is.list() tests if an object is a list but a list can never have a class such as.character()
# The analogy of which type of object and which class of object does not apply

# 3.
# Predict the result:
c(1, FALSE) # will be c(1 , 0)
c("a", 1) # will be c("a", "1")
c(list(1), "a") # will be a list of two, with 1 and a as the first and second elements
c(TRUE, 1L) # will be c(1, 1L)

# 4.
# Because a list is fundamentally different from vectors. It can contain lists
# inside lists and each list be of different class. Hence the argument recursive
# inside unlist(), making sure to take care of any lists inside lists. as.vector()
# cannot handle any situation like that one

# 5.
1 == "1" # because R coerces "1" == "1"
-1 < FALSE  # because R coerces to -1 < 0
"one" < 2  # I think it's because 2 is coerced to character and any logical
           # operation with character will return a FALSE

# 6.
# I don't know. A guess: logicals are the least coercive element in R, making it more efficient
# to have the NA as logical.
