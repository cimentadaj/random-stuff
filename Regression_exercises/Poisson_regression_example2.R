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

## Continue tomorrow: you ended in this command 
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
