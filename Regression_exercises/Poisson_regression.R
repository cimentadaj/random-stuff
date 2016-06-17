## Poisson regression tutorial
# https://onlinecourses.science.psu.edu/stat504/node/169


library(RCurl)
## R has some problems reading in https(it was giving this error: Error in file(file, "rt") : https:// URLs are not supported)
# Using the RCurl packages you can get a workaround: http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
crab <- read.table(text = getURL("https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/crab.txt"))
colnames(crab)=c("Obs","C","S","W","Wt","Sa")

## Let's first explore what count data is. It is the number of ocurrences of an event. For example,
## number of traffic accidents in a specific street, the number of illnesses of a person for a year,
## or the number of deaths in a specific neighborhood.

## Your normally use count data when your DV has a lot of zeros but beware of that, because if you have a LOT
## then you better use zero-inflated models. 


# To remove the column names Obs
crab <- crab[,-1]

# This problem refers to data from a study of nesting horseshoe crabs (J. Brockmann, Ethology 1996);
# see also Agresti (1996) Sec. 4.3 and Agresti (2002) Sec. 4.3. Each female horseshoe crab in the study
# had a male crab attached to her in her nest. The study investigated factors that affect whether the
# female crab had any other males, called satellites, residing near her. Explanatory variables that are
# thought to affect this included the female crab’s color (C), spine condition (S), weight (Wt), and
# carapace width (W). The response outcome for each female crab is her number of satellites (Sa).
# There are 173 females in this study.

## You can think of dependent variable as the number of male crabs(so simply the # of offsprings)

model <- glm(crab$Sa~1, family=poisson(link=log)) ## Here the intercept is the MEAN number of male crabs BUT the Poisson
# coefficients are logarithmic so:
log(mean(crab$Sa)) ##              is the same as the intercept in the model
summary(model) # check it yourself
model$fitted # As you can see, the predicted value is then the mean for every female crab


## Let's add the weight of the female crab as a predictor
model <- glm(crab$Sa~1+crab$W,family=poisson(link=log))
summary(model)
anova(model)


#### to get the predicted count for each observation: 
#### e.g. for the first observation E(y1)=3.810
print=data.frame(crab,pred=model$fitted)
print; exp(0.16405) ## So as the weight of the female crab increases, the number of male crabs will increase
## by 18%

# This means for example that each weight observation will be multiplied by 1.18. e.g. w=26 * 1.18 == 30.68

model$linear.predictors
exp(model$linear.predictors)
# If we look at the scatter plot of W vs. Sa (see further below) we may suspect some outliers, e.g., 
# observations #48, #101 and #165. For example, #165 has W = 33.5, and Sa = 7. But by studying the
# residuals, we see that this is not an influential observation, e.g., standardized deviance residual
# is -0.739 from running rstandard(model)
rstandard(model)

model$fitted[crab$W==26]/model$fitted[crab$W==25] # here you get the predicted values of male crabs
# when the weight is 26 and 25. Naturally the results are possitive and > 1 given that the predicted male crabs
# crab$W==26 are higher than for crab$W==25


#### Based on the residual deviance the model does NOT fit well
#### e.g., 567.88/171 = 3.3209
summary(model)

# We can also see that although the predictor is significant the model does not fit well. Given the value of the
# residual deviance statistic of 567.88 with 171 df, the p-value is zero and the Value/DF=567.88/171=3.321 is much
# bigger than 1, so the model does not fit well. The lack of fit maybe due to missing data, covariates or overdispersion.
1-pchisq(model$deviance, model$df.residual)


plot(crab$W,crab$Sa)
identify(crab$W, crab$Sa)

#### click on the plot to identify individual values
#### identified on the screen and the plot, \#48,101,165

#### Diagnostics measures (like in logistic regression)
#### But these work for ungrouped data too, 
#### as long as there is a variable with counts
#### You can do many more but here are a few indicating a lack of fit

influence(model)
plot(influence(model)$pear.res)
# the predicted values vs the residuals
plot(model$linear.predictors, residuals(model, type="pearson"))

#### To predict a new value
predict(model, type="response") ## this is not working, I don't know why.


#### Let's assume for now that we do not have other covariates
#### and we will adjust for overdispersion 
#### first look at the sample mean and variances 
## e.g., 
tapply(crab$Sa, crab$W,function(x)c(mean=mean(x),variance=var(x)), simplify=F)

# Similar to the logistic regression, you can't violate the assumption of overdispersion.
# overdispersion simply means that although you might have crabs with similar weight, they are quite different
# in the number of male crabs they have.
model.disp=glm(crab$Sa~crab$W, family=quasipoisson(link=log), data=crab)

summary.glm(model.disp)
summary.glm(model.disp)$dispersion ## As you can see the dispersion is slightly above 3 which is much higher than 1.

# But even after relaxing the overdispersion assumption, the model fit is still not a good fit.
1-pchisq(model.disp$deviance, model.disp$df.residual)

# What could be another reason for poor fit besides overdispersion? How about missing other explanatory variables? Can we improve the fit by adding other variables?

#### Adding a categorical predictor
#### This corresponds with crab2.SAS
#### make sure C is a factor

is.factor(crab$C)
crab$C=as.factor(crab$C)
model=glm(Sa~W+C,family=poisson(link=log), data=crab)
summary(model)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

#### to get the same order as you do in SAS

contrasts(crab$C)=contr.SAS(levels(crab$C))
model=glm(Sa~W+C,family=poisson(link=log),data=crab)
summary(model)
anova(model)

#### to get back to the default level

contrasts(crab$C)=contr.treatment(levels(crab$C),base=1)

#### Or you can explicilty code the levels to correspond to SAS
#### Notice that change from C1 to C4 in a number of SA is significant 
#### exp(0.447)=1.54, pvalue=0.0324
#### but if you adjust for overdispersion it's not significant

Sa=crab$Sa
W=crab$W
C1=1*(crab$C==1)
C2=1*(crab$C==2)
C3=1*(crab$C==3)
model=glm(Sa~W+C1+C2+C3,family=poisson(link=log))
summary(model)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

plot(crab$W, model$fitted)

model.disp=glm(Sa~W+C1+C2+C3, family=quasipoisson(link=log))
summary.glm(model.disp)
summary.glm(model.disp)$dispersion
anova(model.disp)

#### Treat Color as a numeric predictor 
#### This corresponds to crab3.SAS

Sa=crab$Sa
W=crab$W
C=as.numeric(crab$C)
model=glm(Sa~W+C,family=poisson(link=log))
summary(model)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

newdt=data.frame(W=0,C=1)
predict.glm(model, type="response", newdata=newdt)

#### This corresponds to to crab5.SAS

width=c(22.69,23.84,24.77, 25.84,26.79,27.74,28.67,30.41)
cases=c(14,14,28,39,22,24,18,14)
SaTotal=c(14,20,67,105,63,93,71,72)
lcases=log(cases)
CrabGrp=data.frame(width,cases,SaTotal,lcases)
model=glm(SaTotal~width,offset=lcases,family=poisson(link=log))
residuals(model)
summary(model)
anova(model)
print=data.frame(CrabGrp,pred=model$fitted)
print

#### create a plot of the results

plot(SaTotal, pch="o", col="blue", main="Plot of Observed and Predicted Sa vs. groups", 
     xlab="Width groups", ylab="Number of Satellites")
points(model$fitted, pch="p", col="red")
legend(6,30,c("obs","pred"), pch=c("o","p"), col=c("blue","red"))
## With this graph you can confirm how accurate the predictions are

model=glm(SaTotal~width,offset=lcases,family=poisson(link=identity))
residuals(model)

#### if you wanted to aggregate from the original data
#### create W as a factor variable with 8 levels

W.fac=cut(crab$W, breaks=c(0,seq(23.25, 29.25),Inf))
numcases=table(W.fac)

#### now compute sample means for Width variable by the cuts
#### and total number of Sa cases

width=aggregate(crab$W, by=list(W.fac),mean)
width
SaMean=aggregate(crab$Sa, by=list(W=W.fac),mean) ## Although W.fac is not in the data.frame it has the same number of rows and it is probably ordered
SaMean
plot(width,SaMean)
SaTotal=aggregate(crab$Sa, by=list(W=W.fac),sum)$x
SaTotal ## The total amount of male crabs by weight groups
