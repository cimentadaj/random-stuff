# 1.
x <- sample(replace = TRUE, 20, x = c(1:10, NA))
# Fixed
x <- sample(c(1:10, NA), 20, replace = TRUE)

y <- runif(min = 0, max = 1, 20)
# Fixed
y <- runif(20, min = 0, max = 1)

cor(m = "k", y = y, u = "p", x = x)
# Fixed
cor(x, y, u = "p", m = "k")

# 2.

f1 <- function(x = {y <- 1; 2}, y = 0) {
    x + y
}

f1()
# x gets evaluated, which defines y to be one and then defines x to be 2 (not assigned to y)
# y = 0 gets substituted by y = 1 in the environment of the function. The function will then add 2 + 1

# This shows the principle of matching arguments. x is matched to x but x creates a y and then y is
# matches to the current y

# Try removing x and you'll see how everything changes

# 3. 

f2 <- function(x = z) {
    z <- 100
    x
}

f2()
# z is created before x is evaluted, thus when x is searched for z already exists and it prints 100
# This is the principle of default and missing arguments.
