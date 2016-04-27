library(UsingR)
library(dplyr)
data(hall.fame)

hall.fame %>%
    group_by(Hall.Fame.Membership) %>%
    summarize(cor(games, HR, use="complete.obs"))

hist(hall.fame$HR)

fit <- lm(HR ~ games, hall.fame) ## An increase of 1 game is equivalent
## with a 0.11 increase in homeruns

plot(hall.fame$games, fit$residuals) ## As the number of games increase
## the number residuals became larger.


fit2 <- lm(HR ~ games, data= hall)
plot(fit2$fitted.values, fit2$residuals) ## So it's not because of the outliers

## Perhaps it's nonlinearities
fit3 <- lm(HR ~ games + I(games^2), data= hall.fame)
plot(fit3$fitted.values, fit3$residuals)

## The R2 increased but it was minimal + the residuals are the same
hall.fame$logHR <- log(hall.fame$HR)
hall.fame$logHR[which(!is.finite(hall.fame$logHR))] = NA

fit4 <- lm(logHR ~ games, data= hall.fame)
plot(fit4$fitted.values, fit4$residuals) ## Here was the problem: it was
## the skewness of the y variable

# Actually, it was the squared independent variable as well

fit5 <- lm(logHR ~ games, data= hall.fame)
plot(fit5$fitted.values, fit5$residuals)


fit6 <- lm(logHR ~ games + I(games^2), data= hall.fame)
plot(fit6$fitted.values, fit6$residuals) # There you go


fit6 <- lm(logHR ~ games + I(games^2), data= hall.fame)

attach(hall.fame)
plot(games,logHR)

fit7 <- lm(logHR ~ games + I(games^2) + Hall.Fame.Membership + games:Hall.Fame.Membership)
plot(fit7$fitted.values, fit7$residuals) # There you go

install.packages("effects")
library(effects)
