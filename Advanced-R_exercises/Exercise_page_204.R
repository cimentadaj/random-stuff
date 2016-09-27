 # 1.
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)

lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x) # Because trims is assigned to the next argument in mean

# 2.
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

lapply(mtcars, scale01) # Applied to every column of a data frame

num <- sapply(iris, is.numeric)
lapply(iris[num], scale01) # Only for numeric columns

# 3.

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

models1 <- lapply(formulas, lm, data = mtcars)

models2 <- vector("list", length(formulas))
for (i in seq_along(formulas)) {
  models2[[i]] <- lm(formulas[[i]], data = mtcars)
}

models1; models2

# 4.

bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[nrow, ]
})

bootstraps1 <- vector("list", 10)
for (i in seq_along(bootstrp)) {
  nrow <- sample(nrow(mtcars), rep = T)
  bootstraps1[[i]] <- lm(mpg ~ disp, data = mtcars[nrow, ])
}

# Without anonymous function:
bootstraps2 <- lapply(bootstraps, lm, formula = mpg ~ disp)

# 5.
rsq <- function(mod) summary(mod)$r.squared
m <- list(models1, models2, bootstraps1, bootstraps2)
lapply(m, function(x) lapply(x, rsq))
