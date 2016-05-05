## On this example I'll plumb deeper on how coefficients change when adding a third variable.
## I'll also take a look at residual diagnostics and outliers.


## Simulation dataset and plot
n<-100;t<-rep(c(0,1),c(n/2,n/2));x<-c(runif(n/2),runif(n/2));
beta0<-0;beta1<-2;tau<-1;sigma<-.2
y<-beta0+x*beta1+t*tau+rnorm(n,sd=sigma)
plot(x,y,type="n",frame=FALSE)
abline(lm(y~x),lwd=2)
abline(h=mean(y[1:(n/2)]),lwd=3)
abline(h=mean(y[(n/2+1):n]),lwd=3)
fit<-lm(y~x+t)
abline(coef(fit)[1],coef(fit)[2],lwd=3)
abline(coef(fit)[1]+coef(fit)[3],coef(fit)[2],lwd=3)
points(x[1:(n/2)],y[1:(n/2)],pch=21,col="black",bg="lightblue",cex=2)
points(x[(n/2+1):n],y[(n/2+1):n],pch=21,col="black",bg="salmon",cex=2)


## The two horizontal lines are the means for the treatment and control respectively(red and blue)
## The treated have a higher value of Y than the control.

## Taking from this graph, we can assume that the units were randomized on X given that they are 
## equally likely to be in different values of X(so they're randomly around x, not following a particular
## pattern)

## When we add the X variable we can see that the difference in intercepts does not change significantly
mean(y[(n/2+1):n]) - mean(y[1:(n/2)]) ## Difference between the horizontal lines
coef(fit)[1]+coef(fit)[3] - coef(fit)[1] ## Difference between the intercepts of treated and controlled

## So even after adding X the difference between the two groups is not significantly changed

## Let's inspect some residuals
data(swiss);par(mfrow=c(2,2))
fit<-lm(Fertility~.,data=swiss);plot(fit)

## The first plot is simply the residuals against the fitted values. The trick here
## is to look for any systematic trend. Remember that in regression we're
## interested in explaining everything that is systematic about Y and everything that is unexplained
## should be mostly noise.

## In this case there doesn't seem to be any strong trends. The Normal Q-Q plot is
## showing how the residuals are distributed. Here they seem to be normally distributed.

## The third plot is simply the same as the first one but with standardized residuals.
## SD residuals are just a way of comparing residuals across models

## Finally, the last one shows us how influential each residual is in changing our model coef
## If any residual would be within the red lines then they are influential.

## Let's have a look at more examples about outliers
n<-100;x<-c(10,rnorm(n));y<-c(10,c(rnorm(n)))
plot(x,y,frame=FALSE,cex=2,pch=21,bg="lightblue",col="black")
abline(lm(y~x))
## Do you think this outlier will have a strong influence over the coef?
## We need to think this through as a fulcrum. That point alone(given that
## its not in the predicted line) will probably have a reasonable influence on all other dots.
fit<-lm(y~x)
round(dfbetas(fit)[1:10,2],3)
## Have a look at the first number: quite different from all other numbers
round(hatvalues(fit)[1:10],3)
## Here as well

## Here is a case where the observation is an outlier, but the LEVERAGE it might have is
## quite low considering that the dot is much close to the prediction.
n<-100;x<-c(10,rnorm(n));y<-c(1,c(rnorm(n)))
plot(x,y,frame=FALSE,cex=2,pch=21,bg="lightblue",col="black")
abline(lm(y~x))

fit<-lm(y~x)
round(dfbetas(fit)[1:10,2],3)## See the influence? it went down from 6 to 1
round(hatvalues(fit)[1:10],3)## See the hat value? It's quite similar to the last one

## Basically the degree of influence of an outlier is measured as its position relative to
## all the dots and the fitted line. If it's far from the dots AND the line, then it will
## probably have a reasonable impact over the slope. 


dat <- read.table("http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt")
summary(lm(V1~.-1,data=dat))$coef ## Everything looks good right?
## simple means for all covariates

fit<-lm(V1~.-1,data=dat);plot(predict(fit),resid(fit),pch='.')
## When you inspect the residuals, that's what happens... there's clearly a systematic pattern.
