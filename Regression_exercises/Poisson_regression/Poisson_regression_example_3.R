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
## deviance, this model is an improvement from the last model. However, noe that there's an exposure variable.
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

## Two things to notice first: hispanics have now virtually the same amount of stops per arrests as hispanics
## and the model seems to disproportionally improve as the deviance goes down almost over 90% of the null model


## Now let's take care of the overdispersion by estimating the same model with a quasipoisson disitribution
fit.5 <- glm(stops~ factor(eth) + factor(precinct),family=quasipoisson,offset=log(arrests),data=frisk2,
             subset=arrests>0)
display(fit.5)
## Wow! overdispersion is almost 21 times more than expected(21.9).
## Much of the precinct coefficients become now insignificant but the white/hispanic coefficients
## still are significant.
