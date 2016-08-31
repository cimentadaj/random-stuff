# Analysing Comparative Longitudinal Survey Data Using Multilevel Models
# RECSM Summer School in Survey Methodology
# Malcolm Fairbrother
# 6 July 2016

install.packages("plm") # a package of "panel data econometrics in R" (by Croissant and Millo)
install.packages("lme4") # this is the first multilevel (random effects) package we'll use

# now we need to make the package available to use:

library(plm)
library(dplyr)
library(arm)

data("Produc", package = "plm") # makes the dataset "Produc" available in the local environment

?Produc # tells you about the dataset we'll use, which comes with the plm package
dim(Produc)
head(Produc)
str(Produc)

# Start by fitting a simple linear model that ignores clustering completely...
mod1 <- lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
display(mod1) 

# So here the models assumes that all observations are independent of each other -- this means
# that the SE's are inflated

# Let's compare that to a model where it estimates the within effect.
# Picture a scatterplot with dots grouped by colors.

# The models subtracts all dots from the overall X mean leading to the overall mean of
# each group to be zero(so centered)

# It then subtracts the each dot from the overall Y mean leading to each group mean to be zero.
# Each "group" mean is at (0,0) in the scatterplot

# Substantively, this means that there is not group differences or between group variation
# because we are forcing each group mean to be in exactly (0,0).

modW <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="within")
# This model is controlling for state even though I'm not specifying the state variable

summary(modW)$coef
# so the pcap coefficient turned from 0.16 to -0.02. When the data is estimated to have
# no differences between groups(so all "clustered" together into 1 group) the relationship
# between pcap and gsp is negative, so more pcap is associated with less gsp.

# The within model is simply "controlling" for each state dummy, meaning that the model
# is estimated free from any differences between states

# Compare the past model with this one containing the state variable
modW2 <- lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.factor(state), data = Produc)
summary(modW2)$coef
# The results are the same for all covariates

# We could do exactly the same for years, meaning that it subtracts each X value for each year
# from the overall X mean and the same for the Y values, assuming that there is no differences between each year.

# We can also add dummies for years, making a "two-way fixed effects model":
modT <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect="twoways")
summary(modT)$coef
# Now we control for states and years, constraining the variance between groups and years

# and in our linear model:
modT2 <- lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.factor(year) + as.factor(state), data = Produc)
display(modT2)

# now fit a "between" effects model only, using the specialised plm function:
modB <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="between")
summary(modB)$coef

# contrast with the "within" models above... what do you see?
# the within slope is negative, so within each state + pcap is associated with - gsp
# However, between states, the states with higher pcap have also higher gsp.
# This estimation is done by calculating
# the average X variable in each group and then estimating a slope out of that. The higher the mean
# for each group, the higher the mean dependent variable


phtest(modB, modW) # a Hausman test (a small p value means it's unlikely that any difference is due to chance)
## The Hausman test means that the within and between models are significantly different. It doesn't mean
# the you should choose one over the other; this depends on your substantive question.


# this next section gives you group-mean centered versions of the covariates
# "by" applies a function to each group separately:
means <- by(Produc, Produc$state, function(x) colMeans(subset(x, select=c(pcap, pc, emp, unemp)))) # Calculate
# the means for each independent variable

head(means)
means <- do.call(rbind, means) # transform the object "means" into a matrix
dim(means)
head(means)
means <- data.frame(state=rownames(means), means) # and now into a data.frame, with a variable that identifies the states
head(means) # You have now the mean for each independent variable for each state
names(means)[2:5] <- c("pcapM", "pcM", "empM", "unempM") # make sure to rename the group means
prod <- merge(Produc, means) # and merge the result back into the original dataset... we'll call the result something new
head(prod)

prod$lpcapM <- log(prod$pcapM) # take the log of the group means
prod$lpcM <- log(prod$pcM)
prod$lempM <- log(prod$empM)

library(lme4)

# now fit a random effects model only with the group means... what do you see?
mlmB <-lmer(log(gsp) ~ lpcapM + lpcM + lempM + unempM + (1 | state), data = prod) # fit a random intercept
summary(mlmB)$coef

# It's really similar to the between states model but differs only because the between model
# computes a weighted average. You can see it as a scatterplot with 5 dots positively linear.
# Each dot is the mean of a group of dots, think of 6 countries. As each mean of X increases
# the corresponding value increases. Like the example below:

# Means for each group plotted in Y ~ X
#   |              %
#   |           +
# Y |        @
#   |     *
#   |  O
#    ---------------------
#             X

prod$lpcap <- log(prod$pcap) # take the log of the original covariates
prod$lpc <- log(prod$pc)
prod$lemp <- log(prod$emp)

# take the difference between the (log-transformed) state-year covariates and their state means:
prod$lpcapD <- prod$lpcap - prod$lpcapM
prod$lpcD <- prod$lpc - prod$lpcM
prod$lempD <- prod$lemp - prod$lempM

# and one non-transformed variable:
prod$unempD <- prod$unemp - prod$unempM

# fit a random effects model only with the differences variables... what do you see now?
mlmW <- lmer(log(gsp) ~ lpcapD + lpcD + lempD + unempD + (1 | state), data = prod)
summary(mlmW)$coef

## It's the same as the within model. So the within model is regressing the difference of each covariate
## from its state mean on the dependent variable. And it's allowing the intercept to vary by group.

# Which would be the same as:
summary(lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.factor(state), data=prod))$coef

# and finally fit a model a random effects model with BOTH the between and within components:
mlmBW <- lmer(log(gsp) ~ lpcapD + lpcD + lempD + unempD + lpcapM + lpcM + lempM + unempM + (1 | state), data = prod)
summary(mlmBW)$coef
## This is really cool. So each variableD is the same as the within and every variableM is the same
## as the between

# now compare the U_j's from the RE and FE between models:
toplot <- cbind(modB$residuals, ranef(mlmB)$state)
#  The first one is the residuals from a between model and the second one is the difference of
#  each state intercept from the overall mean intercept, which is simply the weighted
#  state mean of the log(gsp)

# fit models separately for every state, and extract the coefficient estimates...
# first, using the differenced variables, so these would be the within coefs of each state:
by(prod, prod$state, function(x) lm(log(gsp) ~ lpcapD + lpcD + lempD + unempD, x)$coefficients)
# the intercept in this case is the value of Y for the mean of every X

# now, using the original variables:
by(prod, prod$state, function(x) lm(log(gsp) ~ lpcap + lpc + lemp + unemp, x)$coefficients)
# the intercept is the Y value when each variable is 0

# what's the difference?
# The intercept. 
# Each variable slope is the same, simply the interpretation of the intercept changes.


# now clear the workspace (or just close R, without saving, and re-open it):
rm(list=ls())

###### fourth, if there's time, let's start working with some data from the European Values Study

# download the file "evs-select.csv" from http://bit.ly/29ntWRx, and 
# copy it to:
getwd() # this is where R reads from and writes to, by default (you can use "setwd" to change it)
library(RCurl)
f <- getURL("https://raw.githubusercontent.com/cimentadaj/random-stuff/master/Introduction_Multilevel/Longitudinal-Multilevel-short_course/evs-select.csv")
evs <- read.csv(text=f) # load the data (may take a moment... it's about 13MB)
backup <- evs # the name says it all (just avoids having to re-read the dataset if you want to start over)

class(evs) # data frame
dim(evs) # 166502 by 9
head(evs)

# a040	Learn children at home: religious faith
# e018	Good/bad: more respect for authority
# f063	How important is God in your life
# s002evs 	wave
# s003	Country
# s020 	Survey year
# x001	sex
# x002 	Year of birth respondent
# x003 	Age respondent

names(evs) <- c("childfaith", "auth", "godimp", "wave", "cname", "year", "sex", "born", "age")

dim(evs)
head(evs)
summary(evs)
str(evs)

table(evs$wave)
evs$wave <- as.numeric(evs$wave)
table(evs$wave)

table(evs$cname)
evs$cname <- factor(evs$cname) # this removes unneeeded/empty categories
levels(evs$cname)[levels(evs$cname) %in% "Bosnia Herzegovina"] <- "Bosnia and Herzegovina"
levels(evs$cname)[levels(evs$cname) %in% "USA"] <- "United States"
levels(evs$cname)[levels(evs$cname) %in% "Northern Ireland"] <- "United Kingdom"
levels(evs$cname)[levels(evs$cname) %in% "Great Britain"] <- "United Kingdom"
evs$cname <- factor(evs$cname)
table(evs$cname)

table(evs$childfaith)
evs$childfaith <- ifelse(as.numeric(evs$childfaith) %in% c(1,3,5,6), NA,
                         as.numeric(as.numeric(evs$childfaith)==2))
table(evs$childfaith, useNA="always")

evs$godimp <- ifelse(evs$godimp>0, evs$godimp, NA)

evs$male <- as.numeric(evs$sex=="male")
evs$sex <- NULL # removes the variable

table(evs$year)
class(evs$year) # right now, year is categorical... we want it numeric, so:
evs$year <- as.numeric(levels(evs$year)[evs$year])

evs$time <- evs$year-min(evs$year)
evs$born <- ifelse(evs$born<1880, NA, evs$born)
evs$age <- ifelse(evs$age<15, NA, evs$age) # anything under fifteen seems a little suspicious... let's just set it as missing

plot(table(evs$age)) # a simple and easy graphic
plot(table(evs$age), xlab="Age", ylab="Count") # a simple and easy graphic

# the next part merges this dataset with information about GDP per capita

# install.packages("WDI") # the WDI package connects to the World Bank's "World Development Indicators" website

library(WDI)
wdi <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", start = 1981, end = 2014)
class(wdi$country)
wdi$country <- factor(wdi$country)
levels(evs$cname)[!levels(evs$cname) %in% levels(wdi$country)] # tells us what countries are NOT in the WDI data
levels(wdi$country)[which(levels(wdi$country)=="Macedonia, FYR")] <- "Macedonia" # just change the name
levels(evs$cname)[!levels(evs$cname) %in% levels(wdi$country)] # only one country doesn't match
wdi$GDPpc <- wdi$NY.GDP.PCAP.PP.KD
wdi$lGDPpc <- log(wdi$GDPpc)
wdi <- subset(wdi, select=c(country, year, GDPpc, lGDPpc))
names(wdi)[names(wdi) %in% "country"] <- "cname" # rename the variable
wdi <- wdi[wdi$cname %in% evs$cname,] # select only those rows corresponding to countries in the EVS
evs <- merge(evs, wdi, all.x=T)

head(evs)
