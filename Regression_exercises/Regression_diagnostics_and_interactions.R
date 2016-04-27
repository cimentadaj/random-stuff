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


fit2 <- lm(HR ~ games, data= hall.fame)
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


fit7 <- lm(logHR ~ games + I(games^2) + Hall.Fame.Membership + games:Hall.Fame.Membership, hall.fame) ## Let's look at interactions
plot(fit7$fitted.values, fit7$residuals)

install.packages("effects")
library(effects)

plot(effect("games:Hall.Fame.Membership", fit7,, list(games=c(958,1282,1652,3562))), multiline=TRUE, ylim = c(1.5,7))

## check out the effect command. Here you're saying visualize the interaction games:Hall.Fame.Membership for the values of
## 958, 1282, 1652, 3562 of games with the lines superimposed(with the multiline command)
