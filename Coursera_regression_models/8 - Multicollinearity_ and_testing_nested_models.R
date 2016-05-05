## All models are WRONG, some models are useful.

## An interesting way of thinking about regression is with the known-unknown analogy.
## When talking about regression covariates we have:

## Known-Known: Those which we know are theoretically important and we have them at our disposal
## Known-Unknown: Those which we know are theoretically important but don't have them(due to not measuring
## or difficulty in measuring)
## Unknown-Unknown: Those which we have not idea that matter and we might or might not have them measured

## When thinkking about what to include it would be good to create a list of each of these categories.

# Model fit:
## Everytime we include CORRELATED covariates in the models we're inflating the standard errors and
## the coefficients. This is the typical multicollinearity. We estimate that with the VIF.
library(car)
fit <- lm(Fertility ~ ., data=swiss)
vif(fit)

# The VIF is the variance inflation factor(VIF) and if it's above 5 it means that you have
# a problem of multicollinearity with this variable and others.

## The problem with adding more covariates it that we take up degrees of freedom, they might be correlated
## with other covariates. Always exclude additional covariates that don't bring any explanatory power
## and don't have any theoretical reason to be in the model.

## A good way of avoiding to include many covariates is to think about the research or experimental design
## For this read Firebaugh: Seven rules of Social Research.

## You can check if adding more covariates is meaningfull with the anova function.

fit1 <- lm(Fertility ~ Agriculture, data=swiss)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit3, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1,fit3,fit5)

## The out is like this: first column is the remaining degrees of freedom in each model, RSS is
## the residual sum of squares(so the smaller the better), DF is degrees of freedom
## and Sum of Sq is some of squares

## You can see that the second model decreases the RSS by half, and it is significant.
## The third model as well, but not as much.

## Ideally each model should have 1 additional variable but if you know the dataset then you
## can play around.
