## Confidence intervals and Standard errors

library(UsingR);data(diamond)

##### This you know: beta1, beta2 and residuals(y data points minus the predicted y)
y<-diamond$price;x<-diamond$carat;n<-length(y)
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
######

## Residual variance
sigma<-sqrt(sum(e^2)/(n-2))

## Sum of the squared deviations from x
ssx<-sum((x-mean(x))^2)

## Standard error of th intercept
seBeta0<-(1/n+mean(x)^2/ssx)^.5*sigma

## Standard error for beta1
seBeta1<-sigma/sqrt(ssx)

## Let's talk about this. Sigma in this case is simply the standard deviation of the residuals. So how much they vary.
## The denominator is the square root if the squared deviations.

## From a substantive point of view, the SE is simply diving how much our unexplained varies by how much the predictor
## varies relative to the mean. The lower the residual variance and the higher the predictor variance,
## the more precise the estimate(this is counter intuitive but since you are squaring the predictor variance
## the higher, the lower will be the square).

# The t statistic
tBeta0<-beta0/seBeta0;  tBeta1<-beta1/seBeta1

# The P values
pBeta0<-2*pt(abs(tBeta0),df=n-2,lower.tail=FALSE)
pBeta1<-2*pt(abs(tBeta1),df=n-2,lower.tail=FALSE)

coefTable<-rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable)<-c("Estimate","Std.Error","tvalue","P(>|t|)")
rownames(coefTable)<-c("(Intercept)","x")


## Compare
fit<-lm(y~x)

coefTable
summary(fit)$coefficients

## Now let's talk about confidence intervals

sumCoef<-summary(fit)$coefficients
sumCoef[1,1] + c(-1,1) * qt(.975,df=fit$df) * sumCoef[1,2] ## Confidence interval for intercept
sumCoef[2,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[2,2] ## Confidence interval for slope

## Read it as this: the slope +-1, multiplied by the degrees of freedom and the standard error

predict(fit, interval="confidence") ## for each observations, confidence interval
confint(fit) ## for slope intervals

## I made this to retrieve a table with confidence intervals
reg <- function(model) {
    summary <- summary(model)$coefficients
    confint <- confint(model)
    return(cbind(summary, confint))
}
