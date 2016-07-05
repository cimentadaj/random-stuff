## Ordered and unordered logistic regressions

storage <- read.csv("http://www.stat.columbia.edu/~gelman/arm/examples/storable/2playergames.csv")

# This dataset is an experimental for experimental economics.
# Basically, each student was asked into two rounds of votes. In total,
# each student could use 3 votes distributed among the 2 rounds
# The voting topic would be different for each round and puting more votes
# into a specific round would yield more profits based on the variable value.

# Naturally, students put more votes on the topics which returned more profits.

# Let's do some exploratory analysis
library(dplyr)
library(ggplot2)
library(arm)
library(tidyr)
library(MASS)

storage %>%
    group_by(vote) %>%
    summarise(mean(value))
storage$vote <- factor(storage$vote, ordered = T)

fit.1 <- polr(vote ~ value, data=storage, Hess=T ,method = "logistic")
## The coefficient for value can be interpreted as a simply log odds coefficient.
## The dependent variable is, however, the latent variable with cutoff points 1|2 and 2|3
## The interpretation is then expressed as being more or less likely to be in the higher or lower
## categories.
exp(0.08) ## means that a unit increase in value is associated with an 8% probability increase towards
## the higher votes

# If you had a question about how statisfied are with life, and the options are, non, neutral and very,
# The difference between Non and Very is evidence but between non and neutral it might not be.
# Let's confirm that by checking if the difference of the threholds in significant. If it were not
# significant, then it would be more informative to combine the categories.

coef <- summary(fit.1)$coef[2,1] - summary(fit.1)$coef[3,1]
se <- summary(fit.1)$coef[2,2] - summary(fit.1)$coef[3,2]

abs(coef) - 2 * se # The difference is highly significant as it 2.52 higher than zero.

# Note that in dependent variables which have 10 categories, or simply ambiguous categories, like the difference
# between very very likely and very likely, then one should test if the differences in the thresholds in significant
# Otherwise, code categories together. If there is also a category with just a few observations, it might
# be better to code it together with other categories because the model won't have the necessary power
# to estimate precise differences in thresholds.

probs <- predict(fit.1, newdata = data.frame(value=c(1,27,50,77,100)), type="probs")
probs <- as.data.frame(probs)
probs$cat <- c("1st","2nd","3rd","4th","5th")

## The relationship is pretty linear. The higher the category, the higher the chances of voting three times
## The lower the category the higher the chances of voting only

probs %>%
    gather(Vote,Probs,-cat) %>%
    ggplot(aes(x=cat, y=Probs)) +
    geom_bar(stat="identity") + facet_wrap(~Vote) + xlab(c("Votes")) + ylab("Probability of voting")
## There it is


#### Marginal Effects
# Note that to estimate marginal effects you need to have AT LEAST 2 independent variables
## The reason, I still don't know why

## With this function you can estimate the marginal effect of each predictor on the 
## ordered categories
library(erer)
ocME(w = fit.1)
## Here we see that with each increase in value the chances of being in category 1 are reduced by 1% whereas the
## chances of attaining the third category are increase by 1%. These are log odds, but since they are so close
## to zero, the exp() is practically the same. Try it yourself.
