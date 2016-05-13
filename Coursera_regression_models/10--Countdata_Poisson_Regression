## Count data is are set of ocurrences that happen to an observation/unit. Imagine that
## the each observation is a street and we count the number of accidents in that street. Or the
## unit are days and the counts are visit to a website. On either situation, you have a unit and a cumulative
## occurence of an event. For these specific types of models we use Poisson regressions. 

## When do you use Poisson regression? if each ocurrence of the even doesn not have a natural limit(that is, it is not based
## on a number of independent trials)

## Let's read in Gelman's data on Poisson regression(page 112 - Data Analysis Using Regression and Multilevel/Hierarchichal Models
if (!"arm" %in% installed.packages()) {install.packages("arm")}
library(arm) # for display() function
X <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/police/frisk_with_noise.dat",skip=6,header=TRUE)
names(X)[3] <- "arrests"
X <- aggregate(cbind(stops, arrests) ~ precinct + eth, data=X, sum)
