## Solution to the problem in Data Analysis Using Regression and Multilevel Models from Gelman and Hill.

## Chapter 6 - Multinomial ordered regression - pg 132 problem 2

## Using the 2000 National Election STudy, predict party identification(five-point scale) using ideology and demographics
## using an ordered logit model

library(haven)
library(MASS)
library(dplyr)
nes <- read_dta("http://www.stat.columbia.edu/~gelman/arm/examples/nes/nes5200_processed_voters_realideo.dta")
nes[] <- lapply(nes, unclass)


## Problem A - Summarize the parameter estimates numerically and also graphically
## The party identification has 4 categories instead of 5.
table(nes$partyid3)

nes$partyid3 <- factor(nes$partyid3, labels=c("Democrats","Independents","Republicans","Apolitical"))
nes$gender <- factor(nes$gender, labels=c("Males","Females"))
nes$race <- factor(nes$race, labels=c("White","Black","Asian","Native Am","Hispanic","Other"))
nes$south <- factor(nes$south)
nes$ideo <- factor(nes$ideo, labels=c("Liberal","Moderate","Conservative"))

nes <- nes[!is.na(nes$partyid3),]

summary(nes$partyid3) # More democrats than anything
summary(nes$gender) # More Females

table(nes$partyid3, nes$gender) # As expted, more females than males are democrats. But the gap is much bigger for
# Democrats than for Republics

summary(nes$race)
table(nes$partyid3, nes$race) # Virtually no republicans are blacks.

## Let's exclude the Apolitical category to have an ordered category
nes_upd <- nes[nes$partyid3 != "Apolitical",]
nes_upd$partyid3 <- factor(nes_upd$partyid3)

m1 <- polr(partyid3 ~ ideo + race + age_10, Hess=TRUE, data=nes_upd)
summary(m1)
conf <- confint(m1)

data.frame(names(m1$coefficients), unname(m1$coefficients), low=unname(conf[,1]), high=unname(conf[,2])) %>%
    rename(IV=names.m1.coefficients., coef=unname.m1.coefficients.)  %>%
    ggplot(aes(IV, coef, ymin=low, ymax=high)) + geom_pointrange() + geom_hline(yintercept = 0)

## Problem B - Explain the results from the fitted model

## Let's start in order. As age increases, the chances decrease if being in the higher categories(Republics)
## Conservatives are much more likely than Liberals of being in the higher categories
## Moderates are more likely than liberas of being in the higher categories, but less likely than
## the Conservatives. These two results are in line given that the higher categories are Republicans.

## Asians are more likely of being in the higher categories than Whites but its not
## statistically different from zero. Blacks are significantly less likely than white of being in the higher categories
## Hispanics as well, but less likely than blacks. Native Americans as well, but less likely than Blacks
## and Hispanics.

## The useful here would also be to have a look at the marginal effects of each predictor on each category
library(erer)
tr <- ocME(w = m1)
tr$out <- tr$out[1:3]

tr$out[] <- lapply(tr$out[], function(x) as.data.frame(x))

tr$out$ME.Democrats$low <- NA
tr$out$ME.Democrats$high <- NA
tr$out$ME.Independents$low <- NA
tr$out$ME.Independents$high <- NA
tr$out$ME.Republicans$low <- NA
tr$out$ME.Republicans$high <- NA

low <- numeric(0)
high <- numeric(0)

for (j in 1:length(tr$out)) {
    for (i in 1:nrow(tr$out[[j]])) {
        coef <- tr$out[[j]][i,1]
        se <- tr$out[[j]][i,2]
        low <- c(low,coef + c(-1)*se*qt(0.975,42))
        high <- c(high,coef + c(1)*se*qt(0.975,42))
    }
}

tr$out$ME.Democrats$low <- low[1:8]
tr$out$ME.Democrats$high <- high[1:8]

tr$out$ME.Independents$low <- low[9:16]
tr$out$ME.Independents$high <- high[9:16]

tr$out$ME.Republicans$low <- low[17:24]
tr$out$ME.Republicans$high <- high[17:24]

tr$out$ME.Democrats$name <- row.names(tr$out$ME.Democrats)
tr$out$ME.Republicans$name <- row.names(tr$out$ME.Republicans)
tr$out$ME.Independents$name <- row.names(tr$out$ME.Independents)

full <- full_join(tr$out$ME.Democrats, tr$out$ME.Independents)
full2 <- full_join(full, tr$out$ME.Republicans)
full2$party <- c(rep("Democrat",8), rep("Independ",8),rep("Republican",8))

full2 %>%
ggplot(aes(x=name, y=effect, ymin=low, ymax=high)) +
    geom_pointrange() + geom_hline(yintercept = 0) + facet_wrap(~party)
