# R Data Analysis Examples: Multinomial Logistic Regression

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(tidyr)

ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")

# The data set contains variables on 200 students. The outcome variable is prog, program type.
# The predictor variables are social economic status, ses, a three-level categorical variable
# and writing score, write, a continuous variable. Let's start with getting some descriptive
# statistics of the variables of interest.

with(ml, table(prog, ses))
with(ml, do.call(rbind, tapply(write, prog,
                               function(x) c(M = mean(x), SD = sd(x)))))
# As expted, academic have higher writing skills followed by the general and vocational programs.

ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)

summary(test)
# It is easy to interpret. Pay attention to the coefficient box.
# Each prog category is RELATIVE to the academic category.
# Those with middle SES have less chances of being in the general program relative to the academic program to those with low SES.
# They, however, have higher chances of being in the vocational program relative to the academic program to those with low SES.
# Pay attention to the std.errors though, they're huge.

# Those with high SES have less chances of being in either the general or vocational program
# The SE's are huge nevertheless.

# This is how UCLA interprets it:
# A one-unit increase in the variable write is associated with the decrease in the log odds of being in general program vs. academic program in the amount of .058 (b_13).
# A one-unit increase in the variable write is associated with the decrease in the log odds of being in vocation program vs. academic program. in the amount of .1136 (b_23).
# The log odds of being in general program vs. in academic program will decrease by 1.163 if moving from ses="low" to ses="high"(b_12).
# The log odds of being in general program vs. in academic program will decrease by 0.533 if moving from ses="low"to ses="middle"(b_11), although this coefficient is not significant.
# The log odds of being in vocation program vs. in academic program will decrease by 0.983 if moving from ses="low" to ses="high"(b_22).
# The log odds of being in vocation program vs. in academic program will increase by 0.291 if moving from ses="low" to ses="middle"(b_21), although this coefficient is not signficant.


# Let's exp them
exp(coef(test))

# Let's have a look at the predicted probabilities
head(pp <- fitted(test))

# They sum up to 1
count <- numeric(0); for(i in 1:nrow(pp)) count <- c(count,sum(pp[i,]))
pp <- cbind(pp,count)


toydata <- data.frame(prog=factor(c("academic","general","vocational")), ses=factor(c("low","middle","high")), write=mean(ml$write))
predict(test, newdata = toydata, "probs")
# As your SES increases, the probs of being in academic increase linearly. The jump is quite significant from middle to high SES.

# We can calculate the predicted probabilities for each SES based on different values of write
max(ml$write) - min(ml$write)
toydata2 <- data.frame(ses=rep(c("low","middle","high"), each=37), write=seq(min(ml$write),max(ml$write)))
toydata2 <-cbind(toydata2,predict(test, toydata2, "probs"))

by(toydata2[,3:5], toydata2$ses, colMeans)

toydata2 <- gather(toydata2, program, probabilty, -c(ses,write))

toydata2 %>%
    ggplot(aes(write, probabilty, colour=ses)) +
    geom_line() + facet_grid(~program ~ ., scales="free")

# Diagnostics and model fit: Unlike logistic regression where there are many statistics for performing model diagnostics,
# it is not as straightforward to do diagnostics with multinomial logistic regression models. For the purpose of detecting
# outliers or influential data points, one can run separate logit models and use the diagnostics tools on each model.

# Sample size: Multinomial regression uses a maximum likelihood estimation method, it requires a large sample size.
# It also uses multiple equations. This implies that it requires an even larger sample size than ordinal or binary
# logistic regression.
