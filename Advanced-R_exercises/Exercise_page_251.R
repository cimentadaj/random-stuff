# 1

download_file <- function(url, destfile, ...) {
  download.file(url, basename(destfile), ...)
}

vec_download_file <- function(urls, ...) {
  invisible(lapply(urls, download_file, ...))
}

partial_download <- purrr::partial(vec_download_file, destfile = tempdir())


url_pdf <-
  c(ess1 = "http://www.europeansocialsurvey.org/docs/round1/survey/ESS1_appendix_a4_e06_4.pdf",
       ess2 = "http://www.europeansocialsurvey.org/docs/round2/survey/ESS2_appendix_a4_e03_5.pdf",
       ess3 = "http://www.europeansocialsurvey.org/docs/round3/survey/ESS3_appendix_a4_e03_4.pdf",
       ess4 = "http://www.europeansocialsurvey.org/docs/round4/survey/ESS4_appendix_a4_e05_3.pdf",
       ess5 = "http://www.europeansocialsurvey.org/docs/round5/survey/ESS5_appendix_a7_e04_0.pdf",
       ess6 = "http://www.europeansocialsurvey.org/docs/round6/survey/ESS6_appendix_a8_e02_1.pdf",
       ess7 = "http://www.europeansocialsurvey.org/docs/round7/survey/ESS7_appendix_a8_e03_1.pdf",
       ess8 = "http://www.europeansocialsurvey.org/docs/round8/survey/ESS8_appendix_a8_e01_0.pdf"
  )

partial_download(url_pdf)

# 2

# colwise does three things

# Subsets columns specified in .cols (either a function or a set of named columns)

# Return an empty data frame if you didn't choose any variables

# apply the function to every column and return the summary statistic

my_rowwise <- function (.fun, .cols = true, ...) {
  
  force(.cols)
  
  # 1
  filter <- 
  if (!is.function(.cols)) {
    .cols <- as.quoted(.cols)
    function(df) eval.quoted(.cols, df)
  } else {
    function(df) Filter(.cols, df)
  }
  
  dots <- list(...)
  function(df, ...) {
    stopifnot(is.data.frame(df))
    
    df <- strip_splits(df)
    
    filtered <- filter(df)
    
    if (length(filtered) == 0)  return(data.frame())
    
    out <- do.call("lapply", c(list(filtered, .fun, ...), dots))
    
    names(out) <- names(filtered)
    quickdf(out)
  }
}

# Here I got stuck and only managed to get # 1 part of code to an FO

# 3

as.matrix.function <- function(f) {
  force(f)
  function(...) {
    as.matrix(f(...))
  }
}

as.data.frame.function <- function(f) {
  force(f)
  function(...) {
    as.data.frame(f(...))
  }
}

as.generic.function <- function(generic, f) {
  force(generic)
  force(f)
  function(...) {
    generic(f(...))
  }
}

as.generic.function(as.matrix, lapply)(mtcars, mean)
as.generic.function(as.data.frame, lapply)(mtcars, mean)

# 4

# TO DO

# 5
library(purrr)

# From functionals
# 1
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
unlist(lapply(trims, function(trim) mean(x, trim = trim)))

partial_mean <- partial(mean, x = x)
map_dbl(trims, ~ partial_mean(trim = .x))

# 2

xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)
unlist(Map(function(x, w) weighted.mean(x, w, na.rm = TRUE), xs, ws))

partial_w.mean <- partial(weighted.mean, na.rm = TRUE)
map2_dbl(xs, ws, ~ partial_w.mean(.x, .y))

# 3

rm_na <- function(x, y, identity = 0) {
  
  vals <- c(x, y)
  ind_na <- which(is.na(vals))
  vals[ind_na] <- identity
  
  vals[1] + vals[2]
}

add <- function(x, y, na.rm = FALSE) {
  if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, 0) else x + y
}

r_add <- function(xs, na.rm = TRUE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs)
}

r_add_part <- function(vec, na.rm = TRUE) {
  partial_r_add <- partial(add, na.rm = na.rm)
  
  reduce(vec, partial_r_add)
}

# 4

v_add1 <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  if (length(x) == 0) return(numeric())
  simplify2array(
    Map(function(x, y) add(x, y, na.rm = na.rm), x, y)
  )
}

partial_v_add1 <- function(x, y, na.rm = FALSE) {
  partial_r_add <- partial(add, na.rm = na.rm)
  
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  if (length(x) == 0) return(numeric())
  simplify2array(
    Map(partial_r_add, x, y)
  )
}

v_add1(1:10, 1:10)

partial_v_add1(1:10, 1:10)

# 5

c_add <- function(xs, na.rm = FALSE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs,
         accumulate = TRUE)
}

compact_c_add <- function(xs, na.rm = FALSE) {
  partial_add <- partial(add, na.rm = na.rm)
  partial_reduce <- partial(Reduce, accumulate = TRUE)
  
  partial_reduce(partial_add, xs)
}

c_add(1:10)
compact_c_add(1:10)

# From function operators

# 6
funs2 <- list(
  sum = function(...) sum(..., na.rm = TRUE),
  mean = function(...) mean(..., na.rm = TRUE),
  median = function(...) median(..., na.rm = TRUE)
)

funs2 <- list(
  sum = partial(sum, na.rm = TRUE),
  mean = partial(mean, na.rm = TRUE),
  median = partial(median, na.rm = TRUE)
)
