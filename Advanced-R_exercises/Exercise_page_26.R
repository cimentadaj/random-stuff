# 1.
dim(1:4) # will return NULL because an atomic vector does not have
         # any dimension attribute

# 2.
is.array(matrix(1:4,2,2)) # I thought it would've been FALSE but any arrays are exactly the same
                          # as matrixes unless the array has more th 2 dimensions. 
is.matrix(array(1:6, c(2,3)))
is.matrix(array(1:12, c(2,3,2))) # See?

# 3.
array(1:5, c(1, 1, 5)) # It is an array 5 dimensions
array(1:5, c(1, 5, 1)) # It is an array 3 dimensions
array(1:5, c(5, 1, 1)) # It is an array 3 dimensions
