1.

inner_work <- function(x, y, large = TRUE, na.rm = TRUE) {
  vec <- c(x, y)
  
  if (is.na(x) && is.na(y) || anyNA(vec) & !na.rm) return(NA)
  
  if (anyNA(vec) && na.rm) {
    sort(vec)[1]
  } else {
    sort(vec, decreasing = large)[1]
  }
}

smaller <- function(x, y, na.rm = TRUE) {
  inner_work(x, y, FALSE, na.rm)
}

larger <- function(x, y, na.rm = TRUE) {
  inner_work(x, y, TRUE, na.rm)
}

smaller(2, NA, na.rm = FALSE)

larger(1, 2)
larger(2, 1)
larger(NA, NA)
larger(1, NA)
larger(NA, 1)
larger(NA, NA, na.rm = FALSE)
larger(numeric(), 1)


larger(2, 1, na.rm = FALSE)

smaller(NA, smaller(NA, 11, na.rm = TRUE), na.rm = FALSE)

larger(NA, larger(NA, 2, na.rm = TRUE), na.rm = FALSE)

other_inner <- function(the_vec, which_fun, na.rm) {
  Reduce(function(x, y) which_fun(x, y, na.rm = na.rm), the_vec)
}

my_min <- function(the_vec, na.rm = TRUE) {
  other_inner(the_vec, smaller, na.rm)
}

my_min(1:10)
my_min(sample(200:400))
my_min(c(NA, 2, 1, 5), na.rm = FALSE)

my_max <- function(the_vec, na.rm = TRUE) {
  other_inner(the_vec, larger, na.rm)
}

my_max(1:10)
my_max(sample(200:400))
my_max(c(NA, 2, 1, 5), na.rm = FALSE)




parallel_inner <- function(..., which_fun, na.rm) {
  all_vecs <- list(...)
  
  df <- data.frame(all_vecs)
  
  final_vec <- sapply(seq_len(nrow(df)),
         function(index) which_fun(df[index, , drop = TRUE], na.rm = na.rm))
  
  final_vec
}

my_pmin <- function(..., na.rm = TRUE) {
  parallel_inner(..., which_fun = my_min, na.rm = na.rm)
}

my_pmax <- function(..., na.rm = TRUE) {
  parallel_inner(..., which_fun = my_max, na.rm = na.rm)
}

x <- sample(100)
y <- sample(100)
z <- sample(201:300)
my_pmin(x, y, z)
my_pmax(x, y, z)

row_min <- function(df) {
  sapply(seq_len(nrow(df)),
         function(index) my_pmin(df[index, , drop = TRUE]))
}

row_min(mtcars)

row_max <- function(df) {
  sapply(seq_len(nrow(df)),
         function(index) my_pmax(df[index, , drop = TRUE]))
}

row_max(mtcars)
