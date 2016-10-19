# This function computes the odds ratio from the log odds of an glm object. It only accepts lists
# as the first argument because I didn't want to go into differentiating between single models, several models and lists.
# The output is a stargazer table with the correct odds ratios, the correct standard errors and the previous p values.

# This doesn't apply to svy glms and accepts all the arguments of stargazer.

# Any pull requests are welcome!

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}
