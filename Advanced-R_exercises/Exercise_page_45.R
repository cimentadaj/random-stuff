# 1. 

mod <- lm(mpg ~ wt, data = mtcars)

mod$df.residual
summary(mod)$r.squared
