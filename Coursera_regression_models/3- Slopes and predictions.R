library(UsingR)
data(diamond)

plot(diamond$carat,diamond$price,
     xlab="Mass(carats)",
     ylab="Price(SIN$)",
     bg="lightblue",
     col="black",cex=1.1,pch=21,frame=FALSE)
abline(lm(price~carat,data=diamond),lwd=2)


fit<-lm(price~carat,data=diamond)
coef(fit)

## Easy interpretation: the intercept is the EXPECTED price for a diamond with a weight of 0 carat.
## The slope is the unit increase in price associated with a unit increase in carat weight.
## Remember the coefficient formula: correlation(x,y) * sd(y)/sd(x)
## Let's interpret substantively. You get the ratio of the variances, so that will yield how much y has more variance over x
## or the other way around, if Y has less variance. And finally, you sort of WEIGHT that variance by the 
## level of association(correlation). By multiplying both things you're sort adjusting for this SD by the association level.

cor(diamond$carat,diamond$price) * sd(diamond$price)/sd(diamond$carat) ## Slope

0.8 * sd(diamond$price)/sd(diamond$carat)
0.6 * sd(diamond$price)/sd(diamond$carat)
0.3 * sd(diamond$price)/sd(diamond$carat)
0.1 * sd(diamond$price)/sd(diamond$carat)

## See how it changes.

## Given that the intercept was uninterpretable in the past models, let's center the independent variable.
fit2<-lm(price~I(carat-mean(carat)),data=diamond)
coef(fit2)

## Now the expected value of Y for the AVERAGE carat weight is 500 Singapore Dollars.

## An interesting thing you can do is play around with the metric of the coefficient. For example,
## in this case, a 1 unit increase in diamond weight is a lot. So we can perhaps adapt it to a different metric.

## What if we wanted to know the unit increase in price for 0.5 increase instead of 1 in weight?
## We simply divide by 2

coef(fit2)[2]/2

## We can take it to another level and estimate for every fraction. A 1/3 increase would yield what?
coef(fit2)[2]/3

coef(fit2)[2]/10

coef(fit2)[2]/20

## Predicting diamond prices ia easy when we have the beta coefficient. The beta coefficient is simply the 
## linear constant change that will occur. So we multiply our constant change by each X value.


## I don't understand this COMPLETELY YET, but if you want to predict the price of diamonds based on a new
## set of weights you add the intercept and the slope and the multiply that figure for any weight.
newx<-c(0.16,0.27,0.34)
coef(fit)[1] + coef(fit)[2] * newx

## You could get exactly the same by using the command:
newx2 <- predict(fit, newdata = data.frame(carat=newx))
## This is the predicted price for these weights.

## let's see the predictions in another way.
plot(diamond$carat,diamond$price)
abline(lm(price~carat, data=diamond))
## Actualy values so everything good so far.

## We create a new data frame with the actual weight and the predicted price
newdata = data.frame(carat=newx, predicted=newx2)
points(newdata, col=2, pch=16)
abline(v=0.16, h=335.7381)
abline(v=0.27, h=745.0508)
abline(v=0.34, h=1005.5225)
## That might have been confusing with all the lines but the key take away is that the prediction is simply
## taking the weight in the X axis, increasing vertically until you reach the line and the see what the
## y value is for that specific point in the line.
