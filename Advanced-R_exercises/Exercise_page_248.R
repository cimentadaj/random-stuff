# 1

negative <- function(f) {
  force(f)
  function(...) {
    -f(...)
  }
}

minus <- function(a, b) a - b

minus(5, 4)
negative(minus)(5, 4)

# 2

capture_it <- function(f) {
  force(f)
  capture <- function(cap) as.character(cap)
  function(...) {
    tryCatch(f(...),
             error = capture,
             warning = capture)
  }
}

capture_it(log)("a")
capture_it(mean)("1:10")

lapply(split(mtcars, mtcars$cyl), capture_it(glm), formula = am ~ mpg, family = "binomial")

# 2

file_tracker <- function(f) {
  force(f)
  all_files <- vector("character")
  function(...) {
    old_files <- dir()
    f(...)
    new_files <- dir()
    # Inefficient but quick for now
    (all_files <<- c(all_files, setdiff(new_files, old_files)))
  }
}

dir_create <- file_tracker(dir.create)

dir_create("try1"); dir_create("try2")
unlink("./try3", recursive = TRUE)

file_deleter <- function(f) {
  force(f)
  del_files <- vector("character")
  function(...) {
    old_files <- dir()
    f(...)
    new_files <- dir()
    (del_files <<- c(del_files, setdiff(old_files, new_files)))
  }
}

unlink_r <- file_deleter(unlink)

dir.create("try1")
dir.create("try2")

unlink_r("try1", recursive = TRUE)
unlink_r("try2", recursive = TRUE)

# Much easier

dir.create("first_file")

all_tracker <- function(directory) {
  files <- dir(directory)
  function() {
    deleted <- setdiff(files, dir(directory))
    created <- setdiff(dir(directory), files)
    
    cat("created files: ", created, "\n",
        "deleted files: ", deleted)
  }
}

dir.create("first_file")
tra <- all_tracker(".")
unlink("first_file", recursive = TRUE)

dir.create("third_file")
tra()

unlink("third_file", recursive = TRUE)
