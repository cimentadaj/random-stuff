# Generate model specification
n <- 1000
x <- rnorm(n, 5, 1)
intercept <- 0.33
slope <- 0.21 # small effect size

sds_pos <- seq(0, 10, 0.05)
est <- numeric()
sign <- numeric()

walk(seq_along(sds_pos), function(index) {
  random_noise <- rnorm(n, sd = sds_pos[index])
  y <- intercept + (slope*x + random_noise) + epsilon
  model_summary <- tidy(lm(y ~ x))
  est <<- c(est, model_summary[2, "estimate"])
  sign <<- c(sign, model_summary[2, "p.value"])
})

ggplot(mapping = aes(x = sds_pos, y = est, colour = sign < 0.05)) +
  geom_point()

# Is it more likely to get over or under estimations?
mean(map_dbl(sds_pos, function(index_sds) mean(est[sds_pos > index_sds] > 0.21)), na.rm = T)

# The above simulation only udpates the sd of independent variable.
# This new one updates both the epsilon and the independent variable sd.

sds_pos <- seq(0, 10, 0.05)
sds_neg <- seq(10, 0, -0.05)
est <- numeric()
sign <- numeric()

walk(seq_along(sds_pos), function(index) {
  random_noise <- rnorm(100, sd = sds_pos[index])
  epsilon <- rnorm(100, sd = sds_neg[index])
  y <- intercept + (slope*x + random_noise) + epsilon
  model_summary <- tidy(lm(y ~ x))
  est <<- c(est, model_summary[2, "estimate"])
  sign <<- c(sign, model_summary[2, "p.value"])
})

ggplot(mapping = aes(x = sds_pos, y = est, colour = sign < 0.05)) +
  geom_point()
