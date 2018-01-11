# 1
wrapper_mean <- function() {
  the_dir <- tempfile(fileext = ".txt")
  file.create(the_dir)
  
  calc_mean <- function(x, ...) {
    to_cat <- c(readLines(the_dir), paste0(Sys.time(), " Mean was used!"))
    cat(to_cat, file = the_dir, sep = "\n")
    mean(x, ...)
  }
}

the_mean <- wrapper_mean()
replicate(10, the_mean(1:10))
readLines(environment(the_mean)$the_dir)

# 2
# It checks to see if the function was ran once. If it was, it just returns
# the same result every time

wrapper_median <- function() {
  result <- NULL
  function(x, ...) {
    if (is.null(result)) result <<- median(x, ...)
    result
  }
}

median2 <- wrapper_median()

median2(1:10)
median2(2:20)

# it could be called something like first_result
# or one_call.

# 3
delay_by <- function(by, f) {
  force(by); force(f)
  last_call <- as.numeric(Sys.time())
  function(...) {
  now_call <- as.numeric(Sys.time())
  time_run <- by - (now_call - last_call)
  
  if (time_run > 0) Sys.sleep(time_run)
  last_call <<- as.numeric(Sys.time())
  f(...)
 }
}

f <- delay_by(5, sum)
f(1:10); f(1:10)

# 4

wait_until <- function(spe_time, fun) {
  final_time <- Sys.time() + (spe_time * 60)
  force(fun)
  
  function(...) {
  sec <- as.numeric(final_time - Sys.time())
  if (sec > 0) Sys.sleep(sec)
  fun(...)
  }
}

mean2 <- wait_until(1, mean)
mean2(1:10)
# 5

# Because memoise would work with download_file alone
# so we wrap it in delay_by to avoid downloading the file again

# 6

# It's inefficient because it's copying the entire list
# making it very slow and overcharing RAM with extra 
# data


remember <- function() {
  memory <- list()
  
  f <- function(...) {
    index <- ifelse(length(memory) == 0, 1, length(memory) + 1)
    memory[[index]] <<- unlist(list(...))
    invisible()
  }
  
  structure(f, class = "remember")
}

new_fun <- remember()

purrr::walk(1:10, new_fun)

str(environment(new_fun)$memory)

# 7

f <- function(a, b) {
  force(a); force(b)
  function(x) a * x + b
}

fs <- Map(f, a = c(0, 1), b = c(0, 1))

fs[[1]](3)
