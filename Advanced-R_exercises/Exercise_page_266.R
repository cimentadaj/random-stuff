# 1

eval(quote(eval(quote(eval(quote(2 + 2))))))
# I think it's 4 because the leftmost quote has a valid
# expression and eval simply evaluates it

eval(eval(quote(eval(quote(eval(quote(2 + 2)))))))
# I think it's 4 again because it's the same as
# the one above but with two evals. That's just
# eval(eval(4)) which is 4

quote(eval(quote(eval(quote(eval(quote(2 + 2)))))))
# This one's the same as the above but finishes quite
# a quote. So here I think it's just gonna be the whole
# unevaluated expression: eval(quote(eval(quote(eval(quote(2 + 2))))))

# 2
subset2 <- function(df, expr) {
  new_expr <- substitute(expr)
  df[eval(new_expr, df), ]
}

sample_df2 <- data.frame(x = c(1:10))
subset2(sample_df2, x > 8)

# You need to add drop = F to the subset in subset2

# 3
subset2 <- function(df, expr) {
  new_expr <- substitute(expr)
  eval_expr <- eval(new_expr, df)
  filled_eval <- eval_expr[!is.na(eval_expr)]
  df[filled_eval, , drop = FALSE]
}

sample_df2 <- data.frame(x = c(1:10, NA))
subset2(sample_df2, x > 8)

# 4

subset2 <- function(df, expr) {
  new_expr <- quote(expr)
  print(new_expr)
  eval_expr <- eval(new_expr, df)
  filled_eval <- eval_expr[!is.na(eval_expr)]
  df[filled_eval, , drop = FALSE]
}
# Quote wil literally quote "expr" and it wont find that
# that in df

subset2(sample_df2, x > 8)

# 5

select <- function(df, vars) {
  
  # You substitute the expr
  # so the_vars will be something like
  # mpg:cyl
  the_vars <- substitute(vars)
  
  # this list contains the position of each column
  # so list(mpg = 1, cyl = 2, ...)
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  
  # This I don't understand very well
  # If they vars is mpg:cyl, literally, how can I it be
  # interpted as subseting as a list?
  # var_pos[substitute(mpg:cyl)] wouldn't work
  # how does eval evaluate the call?
  pos <- eval(the_vars, var_pos)
  
  # pos then returns the positions of the variables
  df[, pos, drop = FALSE]
}

select(mtcars, -mpg)
# 6

# evalq quotes and evaluates the argument at the same time.
