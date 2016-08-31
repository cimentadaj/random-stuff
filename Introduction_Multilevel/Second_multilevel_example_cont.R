# Analysing Comparative Longitudinal Survey Data Using Multilevel Models
# RECSM Summer School in Survey Methodology
# Malcolm Fairbrother
# 7 July 2016

# contents:
	# reading in data
	# data frames
	# data manipulation and transformation
	# basic graphing

# NEXT: some data manipulation and transformation
	# recoding variables
	# removing variables

# load the EVS dataset from yesterday ("evs-select.csv") and re-do everything in the code

# first, create a unique indicator for each country-wave (this will be important):
evs$cwave <- as.numeric(evs$cname)*10000 + evs$wave

# now we'll just do a little more data cleaning/recoding

table(evs$auth, useNA="always")
evs$auth <- ifelse(evs$auth=="bad", 0, ifelse(evs$auth=="don't mind", 1, ifelse(evs$auth=="good", 2, NA)))
table(evs$auth, useNA="always")

# now some mean-centering:
wdi$cname <- factor(wdi$cname)
cmeans <- by(wdi, wdi$cname, function(x) c(mean(x$lGDPpc, na.rm=T), mean(x$GDPpc, na.rm=T)))
cmeans <- data.frame(cname=names(cmeans), do.call(rbind, cmeans))
names(cmeans)[2:3] <- c("lGDPpcmean", "GDPpcmean")
evs <- merge(evs, cmeans, all.x=T)
evs$lGDPpcD <- evs$lGDPpc - evs$lGDPpcmean
evs$GDPpcD <- evs$GDPpc - evs$GDPpcmean

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
cwmeans <- by(evs, evs$cwave, function(x) mean(x$auth, na.rm=T))
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
summary(lmer(cwauth ~ GDPpcD + (1 | cname), toplot)) # random intercepts
summary(lmer(cwauth ~ GDPpcD + (GDPpcD | cname), toplot)) # random slopes
# what differences do you notice between these two models?

summary(lmer(cwauth ~ GDPpcD + GDPpcmean + (1 | cname), toplot)) # both between and within effects
summary(lmer(cwauth ~ GDPpcD + GDPpcmean + (GDPpcD | cname), toplot)) # with random slopes
# what's interesting about the coefficients here?

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
