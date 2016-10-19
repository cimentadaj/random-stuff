# This function estimates robust standad error for lm and glm and
# returns coefficients as either logit, odd ratios or probabilities.
# logits are default

# arg x must be linear/glm model.

robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
    require(lmtest)
    require(sandwich)
    
    sandwich1 <- function(object, ...) sandwich(object) * nobs(object) / (nobs(object) - 1) # Function calculates SE's
    mod1 <- coeftest(x, vcov = sandwich1) # apply the function over the variance-covariance matrix
    if (coef == "logit") {
    return(mod1) # return logit with robust SE's
    } else if (coef == "odd.ratio") {
    mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
    mod1[, 2] <- mod1[, 1] * mod1[, 2]
    return(mod1)
    } else {
    mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
    mod1[, 2] <- mod1[, 2]/4
    return(mod1)
    }
}
