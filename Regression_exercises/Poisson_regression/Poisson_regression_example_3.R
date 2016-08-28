library(arm)
library(ggplot2)
frisk <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/police/frisk_with_noise.dat", skip=6, header=T)

names(frisk)[3] <- "arrests"
frisk2 <- aggregate(cbind(stops, arrests) ~ precinct + eth, data=frisk, sum) # aggregate is the same as collapse
fit.1 <- glm(stops~1,family=poisson,offset=log(arrests),data=frisk2,
             subset=arrests>0)
display(fit.1)


tapply(frisk2$stops, frisk2$eth, function(x) c(m=mean(x), sd=sd(x)))
# SD's of 700! this is quite some variation

ggplot(frisk, aes(stops, fill=factor(eth))) + geom_density(alpha=.4)
# In fact there is some overdispersion within each category

fit.2 <- glm(stops~ factor(eth),family=poisson,offset=log(arrests),data=frisk2,
             subset=arrests>0)
display(fit.2)
## Ethinicity 1 is blacks, 2 is hispanics and 3 is whites. So here you can see that whites get stopped 16% less
## than blacks and hispanics get stopped 7% more than blacks. And taking the difference in the
## deviance, this model is an improvement from the last model. However, note that there's an exposure variable.
## the interpretation changes. THe DV is now the ratio between stops per arrests. So whites have
## 16% less stops per arrests than blacks. Hispanics have 7% more stops per arrests than blacks.

fit.3 <- glm(stops~ factor(eth),family=poisson,data=frisk2)
display(fit.3)

## Let's try the same model without the exposure(the exposure can be see here simply as controlling for arrests)
## hispanics have now (exp-0.45) 36% less probability of getting stopped relative to blacks.
## whites have 76% less probability of getting stopped. Something I can interpret from this
## is that hispanics probably get more stops per arrests than blacks, although blacks get more stopped
## in absolute terms.

# Let's check it:
tapply(frisk2$stops, frisk2$eth, mean)
tapply(frisk2$arrests, frisk2$eth, mean)
tapply(frisk2$stops/frisk2$arrests, frisk2$eth, mean)

## Now let's include the precint variable
fit.4 <- glm(stops~ factor(eth) + factor(precinct),family=poisson,offset=log(arrests),data=frisk2,
             subset=arrests>0)

display(fit.4)

## Two things to notice first: hispanics have now virtually the same amount of stops per arrests as blacks
## and the model seems to disproportionally improve as the deviance goes down almost over 90% of the null model

## Benchmark for assessing the fit of model: if a dummy has 3 categories, then the residual deviance should
## decrease by 2 if the variable does not improve the model. If the categorical has 75 categories, then
## the decrease in the residual deviance should be of 74 in the case where the predictor DID not add anything to
## the model. In both cases, the residual deviance decreases much more than those benchmark, meaning that the
## model is improved a lot.
## 

## We need to check for overdispersion. For that, we have several options. First, the way Gelman and Hill
## do it:

yhat <- predict(fit.4, type='response') # predict the probabilities of being stopped
z <- (frisk2$stops-yhat)/sqrt(yhat)

# Overdispersion ratio:
sum(z^2)/(225-77)
# sum of the squared residuals divided by N - the number of predictors
# the variance is 21 times higher than the mean(a severe case of overdispersion)
pchisq(sum(z^2), 225-77)
# p value of 1: meaning that it is highly unlikely that the sum of the squared residuals has such a big
# number. In summary, the overdispersion parameter is significantly different from zero. 

## Second, with package AER:
library(AER)
dispersiontest(fit.4, trafo=1)
# the test is significant, which rejects the null hypothesis that says true alpha is equal to 0
# thus there is overdispersion. See http://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant


## Now let's take care of the overdispersion by estimating the same model with a quasipoisson disitribution

fit.5 <- glm(stops~ factor(eth), family=quasipoisson,offset=log(arrests),data=frisk2)

## Wow! overdispersion is almost 21 times more than expected(21.9). Exactly the same as the one calculated
## manually from above.
## 
## Another way of "controlling" for the overdispersion parameter is to multiply all regression standard errors
## by sqrt(overdispersion parameter), which is exactly the same as the quasipoisson results.
## 
## Much of the precinct coefficients become now insignificant but the white/hispanic coefficients
## still are significant.
## 
## After fitting the models, let's explore its reliability by replicating the predicted values and
## comparing how well it fits the actual data.
## 

# Generate a dummy for each category of eth(I'll explain why this has to be done next)
frisk2$whites <- frisk2$eth == 3
frisk2$hispanics <- frisk2$eth == 2
frisk2$blacks <- frisk2$eth == 1
fit.5 <- glm(stops~ whites + hispanics, family=quasipoisson,offset=log(arrests),data=frisk2)
display(fit.5) # the results are exactly the same as with the factor(eth) variable, just for you to know

# Let's start the simulation
set.seed(1241)


n2 <- nrow(frisk2)
X2 <- cbind(rep(1,n2), frisk2$whites, frisk2$hispanics) # generate a matrix with a 1-repeated variable and
# the two available coefficients in the model display. This is important because in the next chunk of code I will do a
# matrix multiplication where the number of columns of this matrix NEEDS to equal the number of rows in the model
# display(the number of coefficients)

# in the current display(fit.5) there are three coefficients: intercept, whitesTRUE and hispanicsTRUE
# matching the number of columns in the X2 matrix.

y.hat <- frisk2$arrests * exp(X2 %*% coef(fit.5)) # this generates the predicted y.hat, controlling for the
                                                  # using the arrests(Which is the offset)
y.rep <- rpois(n2, y.hat) # then we generate n2(225) poisson distributions of the predicted y.hat

# The mean for the replicated dataset is quite similar to the real number of stops
mean(frisk2$stops)
mean(y.rep)

# the variance, however, is a bit higher, meaning that there is more variability(more uncertainty)
sd(frisk2$stops)
sd(y.rep)

# Let's try and replicate this for 1000 simulations
n.sims <- 1000
sim.1 <- sim(fit.5, n.sims) # this simulates n.sims times(1000) each coefficient
y.rep <- array(NA, c(n.sims,n2)) # create an empty array of 1000 rows and 225 columns(the number of rows in the data)
for (s in 1:n.sims) {
    y.hat <- frisk2$arrests * exp(X2 %*% coef(sim.1)[s, ]) # generate y.hat each row of simulated coeff(so 1000 y.hats)
    a <- y.hat/(222.6-1) # penalize for overdispersion by dividing the y.hat by the overdispersion paramter - 1
    y.rep[s, ] <- rnegbin(n2, y.hat, a) # generate a negative binomial distr for yhat
}

Test <- function(y) mean(y)

test.rep <- rep (NA, n.sims) # empty vector that will containt the mean of the 1000 simulations

for (s in 1:n.sims) {
    test.rep[s] <- Test(y.rep[s,]) # for each of the 1000 simulations of yhat, calculate the mean
}

# Let's compare

summary(test.rep)
summary(frisk2$stops)
# The simulations suggest that the replication data does not capture the variability in the stops
# variable. The Max and Min are really high in the real stops relative to the replication +
# the median and other indicators. There is probably some misspecification(precint variable)
