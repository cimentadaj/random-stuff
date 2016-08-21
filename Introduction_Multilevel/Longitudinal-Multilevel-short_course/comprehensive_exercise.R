# Analysing Comparative Longitudinal Survey Data Using Multilevel Models
# RECSM Summer School in Survey Methodology
# Malcolm Fairbrother
# 6 July 2016

# A comparartive logintudinal data contains a column for a country(for example)
# That country has X number of waves(let's say 3), and then you have 5 subjects
# inside that

# Example:
# Country - Wave - Subject
#   1        1       1
#   1        1       2    
#   1        2       1

# It's good to think about having variation:
# Across countries
# Across Waves
# Across subjects

# Fixed effects model
# If you had a scatterplot without groups, you would fit one summarizing line
# You calculate the mean X and Y for each group and subtract that number
# from the Y mean and then from the X mean, and then estimate a regression line
# Fixed effects eliminate between-group variance(remember that when subrtacting the
# y and x mean from the groups) by clustering all groups closer to each other


# Random model
# Allows us to estimate the within group slope with also between group slope


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
set.seed(9)
x = rnorm(100, mean=5)
e = rnorm(100, sd=0.5)
y = e + x+rnorm(100)
data <- data.frame(y=sort(y),x=sort(x),e, color = factor(rep(c("green","blue","red","yellow","black"),each=20)))
plot(data$x,data$y, col=data$color)

data2 <- data %>% group_by(color) %>% summarise(x=mean(x),y= mean(y)) %>% arrange(x,y)
data2 <- as.data.frame(data2)
data2$color <- as.character(data2$color)

for (i in 1:nrow(data2)) points(x=data2[i,2], y=data2[i,3], col=data2[i,1], pch=16, cex=2)
# I don't know why but the colors are off.

# The models subtracts all dots from the overall Y mean leading to the overall mean of
# each group to be zero(so centered)

data2 <- data %>% group_by(color) %>% mutate(ydiff=y-mean(y), xdiff=x-mean(x))
data2 %>% group_by(color) %>% summarize(mean(ydiff),mean(xdiff))


data %>% group_by(color) %>% summarise(x=mean(x),y= mean(y))
data2 <- as.data.frame(data2)
data2$color <- as.character(data2$color)

plot(x= data2$x, y= data2$y, col=data$color)
for (i in 1:nrow(data2)) points(x=data2[i,2], y=data2[i,5], col=data2[i,1], pch=16, cex=2)

# It then subtracts the each dot from the overall Y mean leading to each group mean to be zero.
# Each "group" mean is at (0,0) in the scatterplot

# Substantively, this means that there is not group differences or between group variation
# in the mean for each group.

modW <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="within")
summary(modW)$coef

# so the pcap coefficient turned from 0.16 to -0.02. When the data is estimated to have
# no differences between groups(so all "clustered" together into 1 group) the the relationship
# between pcap and gsp is negative, so more pcap is associated with less gsp.

# The within model is simply "controlling" for each state dummy, meaning that the model
# is estimated free from any differences between states

modW2 <- lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.factor(state), data = Produc)
summary(modW2)$coef
# The results are the same for all covariates

# We could do exactly the same for years, meaning that it subtracts each X value for each year
# from the overall X mean and the same for the Y values, assuming that there is no differences between each year.

# We can also add dummies for years, making a "two-way fixed effects model":
modT <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect="twoways")
summary(modT)$coef
# Now we "control" for states and years, constraining the variance between groups and years

# and in our linear model:
modT2 <- lm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.factor(year) + as.factor(state), data = Produc)
display(modT2)

# now fit a "between" effects model only, using the specialised plm function:
modB <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="between")
summary(modB)$coef
# contrast with the "within" models above... what do you see?
# the within slope is negative, so within each state the more pcap is associated with less gsp
# However, between states, the higher the pcap the more gsp. This estimation is done by calculating
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
mlmB <-lmer(log(gsp) ~ lpcapM + lpcM + lempM + unempM + (1 | state), data = prod) # fit a random slope
summary(mlmB)$coef

# It's really similar to the between states model

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


# and finally fit a model a random effects model with BOTH the between and within components:
mlmBW <- lmer(log(gsp) ~ lpcapD + lpcD + lempD + unempD + lpcapM + lpcM + lempM + unempM + (1 | state), data = prod)
summary(mlmBW)$coef
## This is really cool. So each variableD is the same as the within and every variableM is the same
## as the between

# now compare the U_j's from the RE and FE between models:
toplot <- cbind(modB$residuals, ranef(mlmB)$state)
# So the first one is the residuals from a between models and the second one is the difference of each state from
# the overall mean

# fit models separately for every state, and extract the coefficient estimates...
# first, using the differenced variables:
by(prod, prod$state, function(x) lm(log(gsp) ~ lpcapD + lpcD + lempD + unempD, x)$coefficients)
# the intercept in this case is the value of Y for the mean of every X

# now, using the original variables:
by(prod, prod$state, function(x) lm(log(gsp) ~ lpcap + lpc + lemp + unemp, x)$coefficients)
# the intercept is the Y value when each variable is 0

# what's the difference?
# The intercept. Each variable slope is the same, simply the interpretation of the intercept changes.


# now clear the workspace (or just close R, without saving, and re-open it):
rm(list=ls())

###### fourth, if there's time, let's start working with some data from the European Values Study

# download the file "evs-select.csv" from http://bit.ly/29ntWRx, and 
# copy it to:
getwd() # this is where R reads from and writes to, by default (you can use "setwd" to change it)

evs <- read.csv("/Users/cimentadaj/Downloads/evs-select.csv") # load the data (may take a moment... it's about 13MB)
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
levels(evs$cname)[which(levels(evs$cname)=="Bosnia Herzegovina")] <- "Bosnia and Herzegovina"
levels(evs$cname)[which(levels(evs$cname)=="USA")] <- "United States"
levels(evs$cname)[which(levels(evs$cname)=="Northern Ireland")] <- "United Kingdom"
levels(evs$cname)[which(levels(evs$cname)=="Great Britain")] <- "United Kingdom"
evs$cname <- factor(evs$cname)
table(evs$cname)

table(evs$childfaith)
evs$childfaith <- ifelse(as.numeric(evs$childfaith) %in% c(1,3,5,6), NA, as.numeric(as.numeric(evs$childfaith)==2))
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

install.packages("WDI") # the WDI package connects to the World Bank's "World Development Indicators" website

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
names(wdi)[which(names(wdi)=="country")] <- "cname" # rename the variable
wdi <- wdi[wdi$cname %in% evs$cname,] # select only those rows corresponding to countries in the EVS
evs <- merge(evs, wdi, all.x=T)

head(evs)


## The interesting thing about modelling multilevel
# Is how does the change in educational attainment, for ex, influences
# the average mathematics score. Two time changing variables, so change explaining change

# But you could also explain this for fixed or time-invariant variables, so fitting
# to slopes one for which educational systems A and one for educational systems B.
# You can see how the relationship changes for these two groups. It's basically
# an interactions between the independent variable and the fixed trait.

# Random slopes allow for there to be covariance between the error for each country_j slope
# and the error for each country_j intercept. If this covariance is positive, for ex,
# countries which are higher at their intercept, have positive slopes

# Allowing the slopes to vary randomly has a toll on the significance. The thing is,
# if the slope varies a lot, because there's so much variation, that the models prefers to
# underestimate the significance, so you would have an insignificant result. 
# You can conclude that there is some contextual thing about this relationship because it varies
# some much between countries.

# As a rule of thumb you should attempt to model a random slope as a robustness check
# because if trully there is a random slope and you're not including it, then your
# SE's will be inflated almost twice as high. You can think of including a random slope
# as misspecification.

# Be careful: You if you're measuring some change in x towards a change in y, you need
# to control for time. It might be that the change is spurious.

# Second: we assume that countries are independiente, but they might be
# non-independence, like France and Italy share a border. Be aware of spillover effects.




#########################
# first, create a unique indicator for each country-wave (this will be important):
evs$cwave <- as.numeric(evs$cname)*10000 + evs$wave

# now we'll just do a little more data cleaning/recoding

table(evs$auth, useNA="always")
evs$auth <- ifelse(evs$auth=="bad", 0, ifelse(evs$auth=="don't mind", 1, ifelse(evs$auth=="good", 2, NA)))
table(evs$auth, useNA="always")

# now some mean-centering:
wdi$cname <- factor(wdi$cname)
cmeans <- by(wdi, wdi$cname, function(x) c(mean(x$lGDPpc, na.rm=T), mean(x$GDPpc, na.rm=T))) # Calculate the mean across all years
cmeans <- data.frame(cname=names(cmeans), do.call(rbind, cmeans))
names(cmeans)[2:3] <- c("lGDPpcmean", "GDPpcmean")
evs <- merge(evs, cmeans, all.x=T) # merge the mean colums to the evs dataset(which has a lot of observations)
evs$lGDPpcD <- evs$lGDPpc - evs$lGDPpcmean # center at the country mean
evs$GDPpcD <- evs$GDPpc - evs$GDPpcmean # center at the country mean

# we're going to work with "auth"

# first, let's plot it against time, three ways
# if you don't want to create a pdf, just omit the lines that begin with "pdf" and "dev.off"
pdf("authyearageborn.pdf", height=4)
par(mfrow=c(1,3))
toplot <- by(evs, evs$year, function(x) mean(x$auth, na.rm=T))
plot(as.numeric(names(toplot)), as.numeric(toplot), pch=19, xlab="Year of Survey", ylab="Mean Deference to Authority", cex.axis=1.25, cex.lab=1.25, ylim=c(1.2,2))
toplot <- by(evs, evs$born, function(x) mean(x$auth, na.rm=T))
toplot <- toplot[names(toplot) %in% seq(quantile(evs$born, 0.025, na.rm=T), quantile(evs$born, 0.975, na.rm=T))]
plot(as.numeric(names(toplot)), as.numeric(toplot), pch=19, xlab="Year of Birth", ylab="", cex.axis=1.25, cex.lab=1.25, ylim=c(1.2,2))
toplot <- by(evs, evs$age, function(x) mean(x$auth, na.rm=T))
toplot <- toplot[names(toplot) %in% seq(quantile(evs$age, 0.025, na.rm=T), quantile(evs$age, 0.975, na.rm=T))]
plot(as.numeric(names(toplot)), as.numeric(toplot), pch=19, xlab="Age", ylab="", cex.axis=1.25, cex.lab=1.25, ylim=c(1.2,2))
dev.off()

# now aggregate and mean-center "auth", by country-wave
cwmeans <- by(evs, evs$cwave, function(x) mean(x$auth, na.rm=T)) # cwave is indicator for country_j in wave_i
cwmeans <- data.frame(cwave=names(cwmeans), cwauth=as.numeric(cwmeans))
evs <- merge(evs, cwmeans)
evs$authD <- evs$auth - evs$cwauth # auth centered by country-wave mean auth

# now aggregate and mean-center "auth", by country
cmeans <- by(evs, evs$cname, function(x) mean(x$auth, na.rm=T))
cmeans <- data.frame(cname=names(cmeans), cauth=as.numeric(cmeans))
evs <- merge(evs, cmeans)
evs$cwauthD <- evs$cwauth - evs$cauth # country-wave auth centered by country mean auth

# let's just rescale GDP a bit (so it's in thousands of $, not just $):
evs$GDPpc <- evs$GDPpc/1000
evs$GDPpcmean <- evs$GDPpcmean/1000
evs$GDPpcD <- evs$GDPpcD/1000

pdf("cauthGDPpcmean.pdf")
par(cex.lab=1.25)
par(cex.axis=1.25)
toplot <- na.omit(unique(subset(evs, select=c(GDPpcmean, cauth, cname))))
toplot$cname <- factor(toplot$cname)
palette(rainbow(length(unique(toplot$cname)))) # sets the colour scheme
with(toplot, plot(GDPpcmean, cauth, col=cname, pch=19, ylab="Mean Deference to Authority", xlab="Mean GDP per capita (thousands of dollars, 1981-2009)"))
abline(lm(cauth~GDPpcmean, toplot), lwd=2)
dev.off()
cor(unique(na.omit(subset(evs, select=c(GDPpcmean, cauth))))) # -0.2954455
summary(lm(cauth~GDPpcmean, toplot))

library(lme4) # load the multilevel package (if you do this more than once it's not a problem)

toplot <- na.omit(unique(subset(evs, select=c(cname, cwave, cwauthD, GDPpcD, GDPpcmean, cwauth))))
palette(rainbow(length(unique(toplot$cname)))) # sets the colour scheme
toplot$cname <- factor(toplot$cname)

pdf("GDPpcDcwauthD.pdf")
par(cex.lab=1.25)
par(cex.axis=1.25)
with(toplot, plot(GDPpcD, cwauthD, col= cname, pch=19, xlab="GDP per capita (Differenced)", ylab="Country-Wave Mean AUTH"))
abline(fixef(lmer(cwauthD ~ GDPpcD + (1 | cname), toplot)), lwd=2)
dev.off()

summary(lmer(cwauthD ~ GDPpcD + (1 | cname), toplot))
summary(lm(cwauthD ~ GDPpcD, toplot)) # this is just a simple (de-meaned) linear/OLS model
# notice anything about these two models?
# They are exactly the same because the random intercept model has no variance of intercepts.
# Everything is exactly the same

summarise(lmer(cwauth ~ GDPpcD + (1 | cname), toplot)) # random intercepts
summarise(lmer(cwauth ~ GDPpcD + (1 + GDPpcD | cname), toplot)) # random slopes
# what differences do you notice between these two models?
# In terms of coefficients, they are practically the same. The slope
# is estimated differently though. In the first model the slope is estimate across all countries
# without distinguishing. In the second one, the calculation is done by averging(weighted) out country
# specific slopes. According to Malcom Fairbrother, the meaning of the inercept here changes
# slightly because the beta coefficient will change. So both things change although in small terms.


summary(lmer(cwauth ~ GDPpcD + GDPpcmean + (1 | cname), toplot)) # both within and between effects
# Within countries there is a positive relationship
# Between countries there is a negative relationship
summary(lmer(cwauth ~ GDPpcD + GDPpcmean + (GDPpcD | cname), toplot)) # with random slopes
# what's interesting about the coefficients here?
# That even after allowing for the slope to vary, the results are significant and the changes are not substantial
# That countries with lower intercepts have more positive slopes

REs <- ranef(lmer(cwauth ~ GDPpcD + (GDPpcD | cname), toplot))$cname # extract the random intercepts and slopes
REs <- t(fixef(lmer(cwauth ~ GDPpcD + (GDPpcD | cname), toplot)) + t(REs)) # sums fixed and random effects

pdf("GDPpcDcwauth.pdf")
par(cex.lab=1.25)
par(cex.axis=1.25)
with(toplot, plot(GDPpcD, cwauth, col= cname, pch=19, xlab="GDP per capita (Differenced)", ylab="Country-Wave Mean AUTH"))
abline(fixef(lmer(cwauth ~ GDPpcD + GDPpcmean + (GDPpcD | cname), toplot))[1:2], lwd=5)
abline(fixef(lmer(cwauth ~ GDPpcmean + GDPpcD + (GDPpcD | cname), toplot))[1:2], lwd=5)
dev.off()

pdf("GDPpcDcwauth2.pdf")
par(cex.lab=1.25)
par(cex.axis=1.25)
with(toplot, plot(GDPpcD, cwauth, col= cname, pch=19, xlab="GDP per capita (Differenced)", ylab="Country-Wave Mean AUTH"))
apply(cbind(REs, seq(nrow(REs))), 1, function(Z) abline(Z[1:2], col=Z[3]))
abline(fixef(lmer(cwauth ~ GDPpcD + GDPpcmean + (GDPpcD | cname), toplot))[1:2], lwd=5)
abline(fixef(lmer(cwauth ~ GDPpcmean + GDPpcD + (GDPpcD | cname), toplot))[1:2], lwd=5)
dev.off()

# the "binomial trick"
head(with(evs, table(cwave, auth))) # see how we can make a table of auth by cwave? 
# we will make auth into a binary variable... either 0/1 or 2
au.bin <- evs[!is.na(evs$auth),] # remove missing data
au.bin <- by(au.bin, au.bin$cwave, function(x) table(as.numeric(x$auth==2))) # 118 cwaves 
au.bin <- data.frame(cwave=as.numeric(names(au.bin)), do.call(rbind, au.bin)) # 118 by 3
rownames(au.bin) <- 1:nrow(au.bin)
au.bin <- merge(au.bin, subset(evs[!duplicated(evs$cwave),], select=c(cname, cwave, year, wave, time, GDPpc, lGDPpc, lGDPpcmean, GDPpcmean, lGDPpcD, GDPpcD, cwauth, cauth, cwauthD))) # 118 by 16

# now we'll fit a multilevel logistic regression with just 118 rows of data, representing 151980 people (sum(au.bin$X0)+sum(au.bin$X1))

# note that we're going to start using glmer, not lmer (glmer is for Generalised linear mixed models)

summary(glmer(cbind(X1, X0) ~ lGDPpcD + lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

# now adding random slopes:
summary(glmer(cbind(X1, X0) ~ lGDPpcD + lGDPpcmean + (1 | cwave) + (lGDPpcD | cname), au.bin, family=binomial))

# but wait... we haven't controlled for time

summary(glmer(cbind(X1, X0) ~ time + lGDPpcD + lGDPpcmean + (1 | cwave) + (lGDPpcD | cname), au.bin, family=binomial))
summary(glmer(cbind(X1, X0) ~ as.factor(wave) + lGDPpcD + lGDPpcmean + (1 | cwave) + (lGDPpcD | cname), au.bin, family=binomial))
# what's happened to the coefficient on lGDPpcD?

# the "binomial trick"... with an individual-level covariate
au.bin <- evs[!is.na(evs$auth),] # remove missing data
au.bin <- by(au.bin, au.bin$cwave, function(x) table(x$male, as.numeric(x$auth==2))) # 118 cwaves 
head(au.bin) # see what we've got
au.bin <- lapply(au.bin, as.numeric)
au.bin <- data.frame(cwave=as.numeric(names(au.bin)), do.call(rbind, au.bin)) # 118 by 3
names(au.bin)[2:5] <- c("X0.female", "X0.male", "X1.female", "X1.male")
au.bin <- reshape(au.bin, direction="long", varying=2:5, sep=".", idvar="cwave")
rownames(au.bin) <- 1:nrow(au.bin)
names(au.bin)[2] <- "gender"
au.bin <- merge(au.bin, subset(evs[!duplicated(evs$cwave),], select=c(cname, cwave, year, wave, time, GDPpc, lGDPpc, lGDPpcmean, GDPpcmean, lGDPpcD, GDPpcD, cwauth, cauth, cwauthD))) # 118 by 16

summary(glmer(cbind(X1, X0) ~ gender + lGDPpcD + lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

summary(glmer(cbind(X1, X0) ~ time + gender + lGDPpcD + lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

summary(glmer(cbind(X1, X0) ~ as.factor(wave) + gender + lGDPpcD + lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

summary(glmer(cbind(X1, X0) ~ as.factor(wave) + gender + lGDPpcD + lGDPpcmean + (1 | cwave) + (lGDPpcD | cname), au.bin, family=binomial))


# now let's try fitting a couple growth curves (interacting time with with lGDPpcmean--a time-invariant property of countries):

# first a control for time (linear):
summary(glmer(cbind(X1, X0) ~ gender + lGDPpcD + time + lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

# growth curve
summary(glmer(cbind(X1, X0) ~ gender + lGDPpcD + time*lGDPpcmean + (1 | cwave) + (1 | cname), au.bin, family=binomial))

# including a random slope for time:
summary(glmer(cbind(X1, X0) ~ gender + lGDPpcD + time*lGDPpcmean + (1 | cwave) + (time | cname), au.bin, family=binomial))
