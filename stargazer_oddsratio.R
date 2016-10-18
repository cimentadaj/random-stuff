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

                 
# Estimates robust standad error for lm and glm and
# returns coefficients as either logit, odd ratios or probabilities.
# logits are default

robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
    require(lmtest)
    require(sandwich)
    
    sandwich1 <- function(object, ...) sandwich(object) * nobs(object) / (nobs(object) - 1) # Function calculates SE's
    mod1 <- coeftest(x, vcov = sandwich1) # apply the function over the variance-covariance matrix
    if (coef == "logit") {
    return(mod1) # return logit with robust SE's
    } else if (coef == "odd.ratio") {
    mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
    return(mod1)
    } else {
    mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
    return(mod1)
    }
}
