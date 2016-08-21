# install.packages("xlsx")

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

# Second handout
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

# Third handout
library(haven)
library(survey)
ip_dataset <- read_dta("https://www.dropbox.com/sh/nw8rb6juql8pk61/AACLVBAd6hqqPHW4fOK5GeTMa/a_indresp_ip.dta")
ip_dataset[] <- lapply(ip_dataset, unclass)
ip_dataset <- ip_dataset[ip_dataset$a_jbhrs >= 0,]

ip_survey <- svydesign(id=~1, strata=~a_strata, data=ip_dataset )

# Now estimate the mean and variance for the number hours worked per week (a_jbhrs) and age (a_dvage),
# first for an SRS design (the Stata default estimate) and accounting for the stratification (using svyset). 

## Mean and variance for the number of hours worked per week
## Assuming SRS
mean(ip_dataset$a_jbhrs) # Match stata
var(ip_dataset$a_jbhrs) # Match stata

## Assuming stratified sampling
svymean(~a_jbhrs, ip_survey) ## Match stata
svyvar(~a_jbhrs, ip_survey) ## Match stata

## Mean and variance for age
## Assuming SRS
mean(ip_dataset$a_dvage) ## Match stata
var(ip_dataset$a_dvage) ## Match stata

## Assuming stratified sampling
svymean(~a_dvage, ip_survey) ## Match stata
svyvar(~a_dvage, ip_survey) ## Match stata


## Design effect
svyvar(~a_dvage, ip_survey)[1]/var(ip_dataset$a_dvage)
svyvar(~a_jbhrs, ip_survey)[1]/var(ip_dataset$a_jbhrs)

## Cluster sampling
# Problems with cluster sampling: the units inside a cluster might be correlated due to the shared environment
# or simply unobserved personal characteristics

# Strata vs clustering
# When you pick stratas, you sample EACH strata. With the clusters you sample an X number of clusters.
# NOTE: I SAID BEFORE THAT YOU NORMALLY SAMPLE ALL ELEMENTS WITHIN A CLUSTER BUT IN PRACTICE THAT NEVER HAPPENS.
# THE REASONS WHY CLUSTERS ARE DIFFERENT FROM STRATA ARE OUTLINED HERE.
# Strata is used for BROAD categories like men and women, age categories, regions, whereas clusters
# are smaller units like schools, neighborhoods, faculties in a university.
# Lastly, you pick strata because you have prior information on the stratas(number of people in the strata
# and overall population number) whereas for the clustering you don't have any prior information
# on the clusters.

# Clusters are called primary sampling units or PSU's. In the survey package it's the id argument.
# IF there's an EPSEM design(equal probabilities) you DON'T have to use weights. Think it through,
# if everyone has the same probability of being picked, by chance, you'll get a representative sample
# of the population and there's no need to weight.

# The mose important thing in sampling design is how it affects our variance. 
# Some designs will give more precision, and the CI's will be smaller, with the SE's.
# The key take away is how the sampling desing reduces our variance.

# You can easily calculate the design effects for cluster sampling as well. The division
# would be the variance of the mean of the overall cluster divided by the variance of a simple
# random sample.

# NOTE: All these variances will differ for different dependent variables. You should pick
# your prefered DV or try several important DV's for all researchers.

# The cluster sample is really dependent on the sampling frame. If you want to cluster
# university departments(sociology, political science, etc..) and you have a list
# of all students inside the department, then you could do a simple random sample
# but it would be better to do a stratified sampling because you could get a REPRESENTATIVE
# sample of ALL departments instead of a couple clusters(departments).

# So whenver you do cluster samplingm, it is because you don't have a good sampling frame
# the ideal thing would be to sample ALL elements within a cluster but in the case
# where you don't have a good sampling frame, you'll need to do a non-probability
# design within each cluster.

# A two-stage cluster design is basically doing a SRS within a cluster
# rather than interviewing everyone withint the cluster(one-stage cluster design)
# However, you do an SRS because you have a sampling frame with all elements, 
# which means you don't have to use weights. But if there's another sampling design
# you might need to use weights inside each cluster.

# All design weights are the inverse of the probability of being picked
# Example: imagine you ahve a probability of picking a strata of 1/2(50%)
# and then inside each cluster you have a probability of being picked of 1/7(14%),
# then your overall probability of being picked is 1/(1/2*1/7).
# The 1/2*1/7 simply gives the probability which is 0.07 and the inverse is 
# 1/0.07 which is about 14.

# Non-response weights are the inverse of the probability of response

# https://onlinecourses.science.psu.edu/stat506/node/23


## 
# Systematic sampling
# It is a way of picking elements into a sample. The way to do it is by picking a systematic pattern
# of sampling the unit(most of the patterns are just to pick an element every k elements). So,
# an example where k is 3. Pick element 1, pick element 4, pick element 7, etc..

# Probability Proportional to Size Sampling (PSS)
# Select first stage units(within a cluster, for example) with
# probabilities proportional to the size of the cluster.
# Naturally, the probability per unit will differ
# per cluster and stage(obviously if stratas have different
# probabilities of selection)


# Fourth handout
## First question
faculty <- read.xlsx("Faculty With Salary.xlsx", sheetIndex = 1)
names(faculty)[5] <- "Salary"
names(faculty)[1] <- "ID"
faculty$NA. <- NULL

set.seed(1)
anyDuplicated(faculty$ID) # No duplicates
faculty_srs <- faculty[sample(1:370, 20), ]

## 2nd question onwards
library(haven)
library(survey)
ip_dataset <- read_dta("https://www.dropbox.com/sh/nw8rb6juql8pk61/AACLVBAd6hqqPHW4fOK5GeTMa/a_indresp_ip.dta")
ip_dataset[] <- lapply(ip_dataset, unclass)
ip_dataset <- ip_dataset[ip_dataset$a_jbhrs >= 0,]

## Survey design with cluster and strata variables
ip_survey <- svydesign(id=~a_psu, strata=~a_strata, data=ip_dataset )

# Now estimate the mean and variance for the number hours worked per week (a_jbhrs) and age (a_dvage),
# first for an SRS design (the Stata default estimate) and accounting for the stratification (using svyset). 

## Mean and variance for the number of hours worked per week
## Assuming SRS
mean(ip_dataset$a_jbhrs) # Match stata
var(ip_dataset$a_jbhrs) # Match stata

## Assuming stratified clustered sampling
svymean(~a_jbhrs, ip_survey) ## Match stata
svyvar(~a_jbhrs, ip_survey) ## Match stata

## Mean and variance for age
## Assuming SRS
mean(ip_dataset$a_dvage) ## Match stata
var(ip_dataset$a_dvage) ## Match stata

## Assuming stratified clustered sampling
svymean(~a_dvage, ip_survey) ## Match stata
svyvar(~a_dvage, ip_survey) ## Match stata

## Design effect
svyvar(~a_dvage, ip_survey)[1]/var(ip_dataset$a_dvage)
svyvar(~a_jbhrs, ip_survey)[1]/var(ip_dataset$a_jbhrs)

## Survey design with cluster, strata and weight variables
ip_survey <- svydesign(id=~a_psu, strata=~a_strata, weights=~a_psnenip_xd, data=ip_dataset )

## Mean and variance for the number of hours worked per week
## Assuming SRS
mean(ip_dataset$a_jbhrs) # Match stata
var(ip_dataset$a_jbhrs) # Match stata

## Assuming stratified clustered sampling with weights
svymean(~a_jbhrs, ip_survey) ## Match stata
svyvar(~a_jbhrs, ip_survey) ## Match stata

## Mean and variance for age
## Assuming SRS
mean(ip_dataset$a_dvage) ## Match stata
var(ip_dataset$a_dvage) ## Match stata

## Assuming stratified clustered sampling with weights
svymean(~a_dvage, ip_survey) ## Match stata
svyvar(~a_dvage, ip_survey) ## Match stata

## Design effect
svyvar(~a_dvage, ip_survey)[1]/var(ip_dataset$a_dvage)
svyvar(~a_jbhrs, ip_survey)[1]/var(ip_dataset$a_jbhrs)


######
# Survey package

## Sampling error occurs when you've surveyed a sample which will give population estimates different
## than if you would've surveyed the whole population. Any SAMPLE will have sampling error. 
## It's different if you're dealing with register data(DK,SWE) because that's the population.

data(api)
srs_design <- svydesign(id=~1, fpc=~fpc, data=apisrs)
svytotal(~enroll, srs_design) # SRS= Simple Random Sample
svymean(~enroll, srs_design)
## The same as:
sum(apisrs$enroll * unique(apisrs$fpc))/ (unique(apisrs$fpc) * 200)
## The formula is every value of Y multiplied by the sampling weight divided by the sampling weight
## multiplied by the number of observations

## If some people have different probabilities of getting picked, then you simply multiply
## the weights of each person by their Y values and divide that by the sum of ALL weights.

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
