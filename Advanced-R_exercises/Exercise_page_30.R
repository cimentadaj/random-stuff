# 1.

df <- data.frame(num = 1:5, letter = letters[1:5], test = sample(c(T,F), 5, replace=T))
# I though it as dimensions and names, but it has: names, row.names
# and class.

# 2.
as.matrix(df) # it applies the same coercion rules as c()

# 3.
emptydf <- data.frame()
dim(emptydf) <- c(0,1); dim(emptydf) # Nop, you can't have a data frame with 1 column and 0 rows
dim(emptydf) <- c(1,0); dim(emptydf) # Nop, you can't have a data frame with 0 column and 1 rows
