library(UsingR)

data(diamond)
y<-diamond$price;x<-diamond$carat;n<-length(y)

fit<-lm(y~x)
e<-resid(fit) ## These are the residuals. These are the vertical difference of every data point from the fit line

yhat<-predict(fit) ## These are the predicted values for each X value in the fit line.

## If you remember then, the residual is simply the subtract of each data point from it's corresponding point
## in the summary line.

sum(abs(e)) ## This equals 1170
sum(abs(y-yhat)) ## The residual is simply the subtraction of each data point from it's corresponding point in the summary line.


max(abs(y-yhat)) ## Maximum residual

## I'm not sure I get this, but this is the residuals minus the residuals, get the absolute value and get the max.
max(abs(e-(y-yhat)))
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x))) ## The same as before.

plot(diamond$carat, fit$residuals)
abline(h=0)

plot(diamond$carat, diamond$price)
abline(lm(price ~ carat, diamond), col="red")
## Take the point 0.15 in X. You can see 3 or 4 data points. If you calculate the residuals, you're gonna have 3 or 4
## numbers. When people talk about residual variation, they talk about the variation of difference of each point from
## the fit line.

## In many of these cases you can visuallize this variance because the data points are so close together.
## Which is why you plot the residuals(so the unexplained) against the predictor.

plot(diamond$carat, fit$residuals)
abline(h=0, col="red")

## With this graph you can see how much the dots are apart from the line(0 because 0 means that there is NO variation;
## that is, the line passes through EACH data point in the scatterplot)


## Be careful when interpreting the residual variation from an X and Y scatterplot.

x<-runif(100,-3,3);y<-x+sin(x)+rnorm(100,sd=.2);
plot(x,y);abline(lm(y~x))

## This looks fine, right?

plot(x,resid(lm(y~x)));
abline(h=0)
## Well here are the residuals. As you can see there is a trend in variation and this would mean that you have
## some unobservables missing correlated with the predictor. 

## To estimate the residual variation you could do it like this:

um((e-mean(e))^2/(length(e)-1))

## or

var(e)

############

## However, for the regression, they calculate it differently.

## The square root of the sum of squared residuals divided by n - 2. This n-2 or n-1 is negligible when having a big sample. 
sqrt(sum(resid(fit)^2)/(n-2))

## so this would be the same as:

summary(fit)$sigma ## or the residual standard error.

## Remember R2? The total variance explained by your predictors? Let's calculate it:

Tot.var <- sum((diamond$price-mean(diamond$price))^2) ## Total sum of the variance of prices(aka, total regression variance)

Resi.var <- sum(fit$residuals^2) ## Total sum of squared residuals(aka, unexplained variance)

Expl.var <- (Tot.var - Resi.var)/ Tot.var ## Explained var divided by the Total var
