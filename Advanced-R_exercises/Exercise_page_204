# Exercises 11.1.2, page 204

1.
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

lapply(trims, function(trims) mean(x, trim = trims))
lapply(trims, mean, x=x) # because the x here is refering to the vector named x and the loop is repeating the trims vector
as the next argument

2.
scale01 <- function(x) {
    rng <- range(x, na.rm=T)
    (x - rng[1]) / (rng[2] - rng[1])
}
# To all columns
mtcars[] <- lapply(mtcars, scale01)

# To numeric columns
n <- which(sapply(iris, is.numeric))
iris[,n] <- lapply(iris[,n], scale01)

3.
formulas <- list(
    mpg ~ disp, # I deleted the two other formulas because they were throwing an error, even by themselves
    mpg ~ disp + wt # try it yourself with: lm(mpg ~ I(1 / disp), data=mtcars)
)

# With a for loop
for (i in 1:length(formulas)) {
    print(lm(formulas[[i]], data=mtcars))
}

# With lapply
(formul <- lapply(formulas,lm, data=mtcars))

4.
# with lapply
bootstraps <- lapply(1:1000, function(i) {
    rows <- sample(1:nrow(mtcars), rep=T)
    return(lm(mpg ~ disp, data=mtcars[rows, ]))
})
# with for loop
bootstraps2 <- vector("list", 1000)
for (i in 1:1000) {
    rows <- sample(1:nrow(mtcars), rep=T)
    bootstraps2[[i]] <- lm(mpg ~ disp, data=mtcars[rows, ])
}


# Without anonymous functions, can you do it?
# With lapply you can't because lm is searching for a X input and lapply can't provide it(because its a sequence from 1:1000)
lapply(1:1000, lm, formula = mpg ~ disp, data = mtcars[sample(1:nrow(mtcars), replace = T), ])

# With a for loop you do can:
for (i in 1:1000) {
    print(lm(formula = mpg ~ disp, data=mtcars[sample(1:nrow(mtcars),replace=T),]))
}


5.
r1 <- sapply(bootstraps, function(x) summary(x)$r.squared)

r2 <- sapply(formula, function(x) summary(x)$r.squared)
