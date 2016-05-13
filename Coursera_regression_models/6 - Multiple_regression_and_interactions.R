## On this script I run some multiply regressions to understand how each coefficient is
## calculated. I go through some examples on how a coefficient changes sign when adding another variable
## Lastly, I explain how to interpret interactions with 2 and 3 levels. To finish, I graph the interactions manually


n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(x, sd=0.01)
ey <- resid(lm(y ~ x2 + x3)) ## So this is the CLEAN variation in Y from x2 and x3
ex <- resid(lm(x ~ x2 + x3)) ## This is the CLEAN variation in X from x3 and x3

## And so we calculate the Beta coefficient using the CLEAN variation
cor(ey,ex) * (sd(ey)/sd(ex))
## Confirm your result with the lm function
coef(lm(y ~ x + x2 + x3)) ## There you go: 1.000280


## Let's create a fake dataset
n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)

## N is the number of days. x2 is a sequence of days from 1 to 100.
## x1 is linearly related to x2(.01*x2 part) but with a little random noise ging from -1 to 1
## Finally, y is a function of -x1 and x2 with some small random noise

coef(lm(y ~ x1))
## if we estimate this model we get a POSITIVE coefficient. Doesn't make sense.. the function of y is -x1
## so it should be negative. 

## Let's inspect residuals.     
plot(predict(lm(y~x1)), resid(lm(y~x1))) ## Everything seems fine(that is, no trend in the residuals)
plot(x2, resid(lm(y~x1))) ## No trend when compared to a possible confounder either. 

## But what if we correlated x1 and x2?
c(x1,x2) ## Uhhhh!! If there's correlation, then there's probably some influence between the two variables

coef(lm(y ~ x1 + x2)) ## Now the coefficient makes more sense: close to -1 which is the linear function of x1.

## You can see it as this:
## if you regress y on x1, the residuals are the UNEXPLAINED variance in Y from x1.
## if you regress y on x1 and x2, x1 is the CLEAN variance of x1 from x2 that explains the clean
## variance of y from x2. If before the regression the sign was + and now -, it means
## that the variance x1 clean of x2 is now negative. So the positive + was due to x2.

cor(resid(lm(y ~ x2)), resid(lm(x1 ~ x2))) * (sd(resid(lm(y ~ x2)))/sd(resid(lm(x1 ~ x2))))


## We're gonna learn about interactions and factor variables now.

summary(lm(Fertility ~ Agriculture, swiss))$coef # So far, so good

swiss$catho <- as.numeric(swiss$Catholic > mean(swiss$Catholic)) ## create a dummy for majority catholic = 1
## majority protenstant = 0

fit <- summary(lm(Fertility ~ Agriculture + factor(catho), swiss))$coef # So catholic provinces had 4.62% more fertility
# than protestant

plot(swiss$Agriculture, swiss$Fertility)
abline(lm(swiss$Fertility ~ swiss$Agriculture), col="black")
abline(a = fit[1], b=fit[2], col="red")
abline(a = fit[1]+fit[3], b=fit[2], col="blue")

## After plotting it we can see that they both have the same slope but Catholics have a greater intercept
## than protestants

fit <- lm(Fertility ~ Agriculture*factor(catho), swiss)
summary(fit)$coef
## The intercept is the intercept for the dummy 0
## The slope for agriculture is now the slope for the 0 category, which is protestant
## The intercept for the 1 category is the intercept + the main effect of the factor variable
## The slope for the 1st category is the slope of 0(Agriculture) + the coef in the interaction term

## Let's see how their slope varies

plot(swiss$Agriculture, swiss$Fertility)
abline(a=fit$coef[1], b=fit$coef[2], col="darkgrey")
abline(a=fit$coef[1] + fit$coef[3], b=fit$coef[2] + fit$coef[4], col="green")

fit <- lm(mpg ~ wt + factor(cyl) + wt:factor(cyl), mtcars)

## Exactly the same: the intercept is intercept for the 0 category
## The slope of wt is the slope for the 0 category
## The intercept for the 1st factor is the intercept + its main effect
## It's slope is the slope for factor 0 + the interaction for the 1st factor
## the intercept for 2nd factor is the intercept + its main effect
## the slope is the slope for the 0 factor + the interaction term for the 2nd factor

## visualize it
plot(mtcars$wt,mtcars$mpg)
abline(fit$coef[1], fit$coef[2])
abline(fit$coef[1] + fit$coef[3], fit$coef[2] + fit$coef[5],col="red")
abline(fit$coef[1] + fit$coef[4], fit$coef[2] + fit$coef[6], col="blue")


### These three functions portray how adding or excluding correlated and uncorrelated regressions changes the coefficients.
simbias <- function(seed=8765){
    # The default seed guarantees a nice histogram. This is the only
    # reason that accepting the default, x1c <- simbias(), is required in the lesson. 
    # The effect will be evident with other seeds as well.
    set.seed(seed) 
    temp <- rnorm(100)
    # Point A
    x1 <- (temp + rnorm(100))/sqrt(2)
    x2 <- (temp + rnorm(100))/sqrt(2)
    x3 <- rnorm(100)
    # Function to simulate regression of y on 2 variables.
    f <- function(k){
        # Point B
        y <- x1 + x2 + x3 + .3*rnorm(100)
        # Point C
        c(lm(y ~ x1 + x2)$coef[2],
          lm(y ~ x1 + x3)$coef[2])
    }
    # Point D
    sapply(1:150, f)
}

# Illustrate the effect of bogus regressors on residual squared error.
bogus <- function(){
    temp <- swiss
    # Add 41 columns of random regressors to a copy of the swiss data.
    for(n in 1:41){temp[,paste0("random",n)] <- rnorm(nrow(temp))}
    # Define a function to compute the deviance of Fertility regressed
    # on all regressors up to column n. The function, deviance(model), computes
    # the residual sum of squares of the model given as its argument.
    f <- function(n){deviance(lm(Fertility ~ ., temp[,1:n]))}
    # Apply f to data from n=6, i.e., the legitimate regressors,
    # through n=47, i.e., a full complement of bogus regressors.
    rss <- sapply(6:47, f)
    # Display result.
    plot(0:41, rss, xlab="Number of bogus regressors.", ylab="Residual squared error.",
         main="Residual Squared Error for Swiss Data\nUsing Irrelevant (Bogus) Regressors",
         pch=21, bg='red')
}

# Plot histograms illustrating bias in estimates of a regressor
# coefficient 1) when an uncorrelated regressor is missing and
# 2) when a correlated regressor is missing.
x1hist <- function(x1c){
    p1 <- hist(x1c[1,], plot=FALSE)
    p2 <- hist(x1c[2,], plot=FALSE)
    yrange <- c(0, max(p1$counts, p2$counts))
    plot(p1, col=rgb(0,0,1,1/4), xlim=range(x1c), ylim=yrange, xlab="Estimated coefficient of x1",
         main="Bias Effect of Omitted Regressor")
    plot(p2, col=rgb(1,0,0,1/4), xlim=range(x1c), ylim=yrange, add=TRUE)
    legend(1.1, 40, c("Uncorrelated regressor, x3, omitted", "Correlated regressor, x2, omitted"),
           fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
}

