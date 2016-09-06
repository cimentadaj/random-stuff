# 1.
# By sampling randomly the existing columns of a data frame, for example.
str(mtcars[, sample(ncol(mtcars))])

# Yes, you can randomly permute both rows and columns
mtcars[sample(nrow(mtcars)), sample(ncol(mtcars))]

# 2.
# Random sampling
m <- 5
mtcars[sample(nrow(mtcars), m), ]

# Contiguous
first <- sample(nrow(mtcars) - 5, 1)
last <- first + 5
select <- first:last

mtcars[select, ]

# 3.
mtcars[, order(names(mtcars))]
