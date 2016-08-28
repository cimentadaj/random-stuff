## Second tutorial on Poisson regression

# http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm

install.packages(setdiff(c("ggplot2","sandwich","msm"), installed.packages()))

require(ggplot2)
require(sandwich)
require(msm)


# Example: The number of awards earned by students at one high school. Predictors of the number of awards
# earned include the type of program in which the student was enrolled (e.g., vocational, general or academic)
# and the score on their final exam in math.

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
str(p)

p <- within(p, {
    prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
    id <- factor(id)
}) ## Tansform some prog to factor which is the type of program and id into a factor

summary(p)
with(p, tapply(num_awards, prog, function(x) c(Mean=mean(x),SD=sd(x))))

## Poisson regression has the assumption of overdispersion
# which means that the sqrt root of the mean should be equal to the standard deviation.
# NOTE: they don't have to be exactly EQUAL but close to each other.
# Overdispersion here means that even if you ahve students within a similar program(like Academic)
# you have quite some variation of num_awards. In this case it looks like there is a lot of variation.
# In fact, after calculating the SD of the mean, the standard deviation is always higher that the mean. 

# When to use Poisson regression? First, when you have count data. Count data simply means
# the number of successes of something(deaths, illnesses, prison breaks :), normally for a specific setting, like a county,
# a school or a district). Additionally, the successes should not have a natural limit, i.e. there's no such
# thing as a limit of deaths but there might a limit to the successes of your score in a test.

ggplot(p, aes(num_awards, fill=prog)) + geom_histogram(binwidth=.5,position="dodge")
## For example here, we can see the high variation of the number of awards for the Academic program and the
## lower variation for the other tracks(however, both higher then the mean num of awards per program)

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
# Here the interpretation is as follows:
exp(m1$coefficients)
# Those in the Academic program have 195% more number of awards than the students in the general program.
# Those in the Vocational program have 44% more awards than the ones in general education.
# Finally, as the grades in math by one point, the number of awards will increase by 7%.

## just to check the overdispersion parameter
model.disp <- glm(num_awards ~ prog + math, family=quasipoisson(link=log), data=p)

# Cameron and Trivedi (2009) (Cameron, A. C. and Trivedi, P. K. 2009. Microeconometrics Using Stata. College Station, TX: Stata Press.)
# suggest to use robust SE's when there is a slight violation of the overdisperion assumption.

# Robust SE's are not as straightforward in R as in Stata, so here is the way to calculate it with the sandwich package.

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = round(2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),2),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

# Now let's go back to the glm output
summary(m1)
# If the model is well specified then the  deviance residuals should be normally distributed
# the median of the deviance residuals should be 0 if it were to be normally distributed.

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test
# is not statistically significant. If the test had been statistically significant, it would
# indicate that the data do not fit the model well. In that situation, we may try to determine
# if there are omitted predictor variables, if our linearity assumption holds and/or if there
# is an issue of over-dispersion.

m2 <- update(m1, . ~ . - prog)

anova(m2,m1, test="Chisq") # a significant improvement

# Let's create a table with odds ratios, robust SE's and the CI's.
cbind("Odds ratios"=exp(m1$coefficients), "Robust SE"=std.err, LL=exp(m1$coefficients - 1.96 * std.err),
      UL=exp(m1$coefficients + 1.96 * std.err))


## Let's predict some values. For example, what are the expected counts for each program type holding
## math score at its overall mean?
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
# predict from the model, the new dataset and it's a response count.
predict(m1, s1, type="response", se.fit=TRUE)

## These predictions are interpreted as the number of events for category 1 is 0.21, for category 2 is 0.62
# and lastly for category 3 is 0.30. Each with it's associated SE.

## Let's try the same holding math in a higher quantile
(s2 <- data.frame(math = quantile(p$math, probs = 0.75),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
# predict from the model, the new dataset and it's a response count.
predict(m1, s2, type="response", se.fit=TRUE)


## Let's do that for the original dataset
## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]


## create the plot
ggplot(p, aes(x=math,y=phat, colour=prog)) +
    geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
    geom_smooth() +
    xlab("Math Score") +
    ylab("Expected number of awards")
## Here the interpreation is straight forward: for the academic program the higher the math scores, the higher
## the predicted number of awards. For the general program the relationship is much weaker.

# THINGS TO CONSIDER:

# When there seems to be an issue of dispersion, we should first check if our model is appropriately specified,
# such as omitted variables and functional forms. For example, if we omitted the predictor variable prog in the
# example above, our model would seem to have a problem with over-dispersion. In other words, a misspecified model
# could present a symptom like an over-dispersion problem.

p1 <- summary(glm(num_awards ~  math, family = "poisson", data=p))
p2 <- summary(glm(num_awards ~  math + prog, family = "quasipoisson", data=p))
# Compare how one model excluding is more overdispersed than the other one. Omitted variables contribute
# to overdispersion

# Assuming that the model is correctly specified, the assumption that the conditional variance is equal to the
# conditional mean should be checked. There are several tests including the likelihood ratio test of over-dispersion
# parameter alpha by running the same model using negative binomial distribution. R package pscl (Political Science 
# Computational Laboratory, Stanford University) provides many functions for binomial and count data including odTest
# for testing over-dispersion. But this test only works for the MASS package.

# One common cause of over-dispersion is excess zeros, which in turn are generated by an additional data generating process.
# In this situation, zero-inflated model should be considered.

# If the data generating process does not allow for any 0s (such as the number of days spent in the hospital),
# then a zero-truncated model may be more appropriate.

# Count data often have an exposure or offset variable. The offset is a variable that you include
# so you can interpret the coefficient RELATIVE to this exposure variable. If you're studying police
# stops on ethnic groups, then you can include an offset which could be the amount of cars passing through the inter
# section. With our example, an exposure could be last year's number of awards.

# I create a ficticious varibale with last year's number of awards
set.seed(4)
p$lastyear <- sample(1:3, length(p$num_awards), replace=T)
ggplot(p, aes(num_awards, lastyear, colour=prog)) + geom_point(alpha=.9, position = position_jitter(h=0.2))

# I include it in the model  as an offset
summary(glm(num_awards ~ prog + math, family = "poisson",offset=log(lastyear), data=p))
## Here according to Gelman, page 113, the coefficients are interpreted as this: 
# progAcademic has disproportionately more number of awards relative to last years, as compared to those in the
# general program

## More intuitively, Y is divided by the offset, so the dependent variable is now a rate of occurence of the event.
## So now the dv is the amount of times the num of awards occurd relative to last years. So progAcademic means
# that they have more number of awards relative to last years, compared to general. This makes sense
# considering that I only sampled from 1:3 in last years variable where number of awards goes up to 6
# This helped me understand it: http://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

# The outcome variable in a Poisson regression cannot have negative numbers, and the exposure cannot have 0s.

# Many different measures of pseudo-R-squared exist. They all attempt to provide information similar to that
# provided by R-squared in OLS regression, even though none of them can be interpreted exactly as R-squared
# in OLS regression is interpreted. For a discussion of various pseudo-R-squares, see Long and Freese (2006)
# or our FAQ page What are pseudo R-squareds?.

# Poisson regression is estimated via maximum likelihood estimation. It usually requires a large sample size.
