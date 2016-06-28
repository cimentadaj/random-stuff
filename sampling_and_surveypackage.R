install.packages("xlsx")

library (survey)
library(foreign)
library(RCurl)
library(xlsx)

setwd("/Users/cimentadaj/downloads")

## Sampling probabilities with Prof. Tarek Al Baghal

# sampling error is simply when a survey statistics is different from the one you would've
# gotten if you surveyed everyone in the population.

# Any sample will have sampling error. As long as this sampling error is not BIASED, you're fine.
# you can get a slightly different statistics but based on luck or variability.

# Three principles of survey samples
# Realism - your sample reflects the population
# Randomization - the probability of picking someone is random and based on judgement
# Representation - your sample is representative of the population

# any sample statistic: mean, sd, whatever has an assumption: the sample is randomly picked
# because otherwise the mean will be biased. It's an implicit assumption and not everyone
# remembers it.

# Stratification is dividnig groups and SAMPLING a portion of that group, whereas
# a cluster means that EVERYONE in that group will be picked. If I clustered women
# in a class, everyone will be surveyed whereas if it would've been stratification
# I would've sampled SOME women.

# Probability sample
# All units have a known, non-zero, probability of selection
# You pick them using some mechanism of randomness or chance

# Non-probability sample
# Units have an unknown probability of selection
# Method for selection involves judgement and NOT randomness

# Target population: what you want to study: Working age people in Barcelona, households, businesses

# Survey population: The people you are going to get(for example, you're not gonna get prisoners
# although they're in their working age or tourists)

# Epsen means equal probability of selection
# Non-epsen means non-equal probability of selection
# When you have non-equal probability of selection, you need to weight.

# Simple random sample (SRS)
# Fixed sample size n from population N
# without replacement
# Randomly picked
# Units are not grouped into strata nor clusters
# In EPSEM(Equal Probability of Selection Method) there is no need to use weights.
# The sample is self-weighted because every unit has the same prob of selection

# R, SPSS, etc.. when you calculate a mean, variance, regression and so on, it assumes that
# the sample is a simple random sample, which is highly unlikely given that almost no surveys
# are simple randon samples.

# Sampling variance = draw all possible samples of a population and calculate the mean
# you would want your means to be less different as they can.

money <- sample(40:100,10000, replace = T)
vec <- numeric(0)
for (i in 1:10000) vec[i] <- mean(sample(money, 2))

mean(money) 
mean(vec) # Practically the same
hist(vec, breaks=25)


# fpc = finite population correction
# you estimate the variance for 1 - f where f is n/N. 

# this is not normally used because you use this when you have at least sampled 5% of the population
# any country survey will never sample that many people

try <- sample(money,2)
((1 - 2/10000)/10000) * var(try)
sd(money)

# Estimating a sample mean, probabilistically, will give is the population mean.
# The central limit theorem says that calculating the mean for all permutations of a sample
# will give the population mean. And if we calculate the sample mean the chances are
# I will get the population mean. Confirm it with the example in line 59.

# The 95% CI will be a function of the variance of your sample.
# The higher the variance(std dev) the wider the 95% range.
# If you increase your sample, the std. dv will always be smaller and thus your CI as well.


faculty <- read.xlsx("Faculty With Salary.xlsx", sheetIndex = 1)
names(faculty)[5] <- "Salary"
names(faculty)[1] <- "ID"
faculty$NA. <- NULL

set.seed(1)
anyDuplicated(faculty$ID) # No duplicates
faculty_srs <- faculty[sample(1:370, 20), ]
mean(faculty_srs$Salary) ## A bit off
mean(faculty$Salary) ## A bit off

sum((faculty_srs$Salary - mean(faculty_srs$Salary))^2/(nrow(faculty_srs)-1))
var(faculty_srs$Salary)

((1 - 20/370)/370) * var(faculty_srs$Salary) # Sample mean variability

sqrt(((1 - 20/370)/370) * var(faculty_srs$Salary)) # Standard error of the mean


######
# Survey package

data(api)
srs_design <- svydesign(id=~1, fpc=~fpc, data=apisrs)
svytotal(~enroll, srs_design) # SRS=Simple random sample
svymean(~enroll, srs_design)


srs_design <- svydesign(id=~1, weights = ~pw, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)


svytotal(~stype, srs_design)
means <- svymean(~api00+api99, srs_design)
svycontrast(means, c(api00=1,api99=-1))

srs_design <- update(srs_design, apidiff=api00-api99)
srs_design <- update(srs_design, apipct = apidiff/api99)

svymean(~apidiff+apipct, srs_design)


strat_design <- svydesign(id=~1, strata=~stype, fpc = ~fpc, data=apistrat)
svytotal(~enroll, strat_design)
svymean(~enroll, strat_design)
svytotal(~stype, strat_design)


data <- read.dta("http://r-survey.r-forge.r-project.org/svybook/Adult.dta")
chris_data <- data

chris <- svrepdesign(variables = chris_data[,1:418],
                     repweights = chris_data[,420:499],
                     weights = chris_data[,419],
                     combined.weights = T,
                     type="other",
                     scale = 1,
                     rscale = 1)


p <- getURL("https://raw.githubusercontent.com/xcodevn/SADP/master/nwts/nwts-share.csv")
popn <- read.csv(text = p)

cases <- subset (popn, relaps==1)
cases$wt <- 1
ctrls <- subset(popn ,relaps==0)
ctrls$wt <- 10
samp <- rbind(cases, ctrls[sample(nrow(ctrls), 325) ,]) 

fivesurv <-function(time , status ,w){
    scurve <- survfit(Surv(time,status)~1, weights=w)
    scurve$surv[min(which(scurve$time > 5))]
}

des <- svydesign(id=~1, strata=~relaps, weights=~wt, data=samp)
jkdes <- as.svrepdesign(des)
withReplicates(jkdes, quote(fivesurv(tre1, relaps, .weights)))

svyquantile(~bmi_p, design=chris,quantiles=c(0.25,0.5,0.75))

svyquantile(~api00, strat_design, c(0.25, 0.5, 0.75) ,ci=TRUE)

tab<-svymean(~interaction(ins,smoking,drop=TRUE),chris)

row.names(t) <- 1:nrow(t)
