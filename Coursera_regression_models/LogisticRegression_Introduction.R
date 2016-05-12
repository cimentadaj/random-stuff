
## Let's visualize how logistin regression works.
x<-seq(-10,10,length=1000)
manipulate(
    plot(x,exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)),
         type="l",lwd=3,frame=FALSE),
    beta1=slider(-2,2,step=.1,initial=2),
    beta0=slider(-2,2,step=.1,initial=0)
)

## You can see the curve becoming more steep as the values of x decrease or increase
## The whole idea behind this line is that is summarizes the probability of achieving Y
## for different values of X. If you would put the beta1 in the highest value, we see that as x
## passes around the zero threshold, the odds jump really high that y values will be 1.

temp <- tempfile()
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              ,destfile=temp,method="curl")
load(temp)
head(ravensData)

logRegRavens<-glm(ravensData$ravenWinNum~ravensData$ravenScore,family="binomial")
summary(logRegRavens)

## This is how R calculates the predicted probabilities.
## In a nutshell, we multiply each value of X by the "slope", add the intercept
## (just as if it was an equation), calculate the exponentiation of that and divide
## by 1 + exponentiation of the same thing
ravensData$newpr <- exp((ravensData$ravenScore * 0.10658) + (-1.68001))/(1+exp((ravensData$ravenScore * 0.10658) + (-1.68001)))

## Compare our calculations with R's calculations
View(cbind(ravensData[,c("ravenScore","newpr" )],logRegRavens$fitted.values))
## They're virtually the same, but because I didn't use all decimals they don't match exactly.

## How would you obtain odds ratios?
exp(logRegRavens$coefficients)

exp(confint(logRegRavens))

## You can use ANOVA just as in OLS and compare nested models.
logRegRavens2<-glm(ravensData$ravenWinNum~ravensData$ravenScore + ravensData$ravenWin ,family="binomial")
anova(logRegRavens, logRegRavens2, test = "Chisq")
## As you can see, it is extremely a much better fit as it lower the deviance to 0.
