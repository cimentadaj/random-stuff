## How to calculate covariances, correlations, variances, slopes, standard deviations.

## The mean of a vector can be expressed as this:
(x1 + x2 + x3 + x4 +...xN) * 1/n

(1 + 2 + 2 + 1 + 4 + 5) * 1/6

## The least squared deviations can be expressed as this:

sum((Xi - mu)^2) # where mu is the overall population mean

x <- c(1,2,2,1,4,5)
sum((x - mean(x))^2) ## Why do you square it? Because many of the deviations will be
## negative and thus be squaring it everything will be positive.


## The variance is expressed as:

x <- c(1,2,2,1,4,5)
sum((x-mean(x))^2)/(length(x)-1) ## you can read it as: subtract each observation
## from its mean, square it and the add all of it together. That will be the squared
## deviations from the mean. Divide that by N-1.

## The standard deviation is simply the square root of the variance.
sqrt(sum((x-mean(x))^2)/(length(x)-1))

## Covariance can be expressed as:

sum((x-mean(x)) * (y-mean(y)))/(length(x)-1)
# This means: subtract each datapoint from its mean(both for X and Y) and multiply it.
# This will give you the multiplication of the deviations of X and Y from its
# respective means. Then add this multiplication and divide it by N - 1.

# Your basically saying, the higher the multiplication, the more the covariance.
# You divide to control for sample size and you get the average covariance.


## The correlation can be expressed as this:

    (sum(((x-mean(x)) * (y-mean(y)))/(length(x)-1)))/
    ((sqrt(sum((x-mean(x))^2)/(length(x)-1))) *
    (sqrt(sum((y-mean(y))^2)/(length(y)-1))))

## Let's go part by part: 
## 1) 

(sum(((x-mean(x)) * (y-mean(y)))/(length(x)-1))) ## This is the multiplication of
## the sum of deviations divided by the sample size - 1. Whic in simple terms
## is the covariance. 

## 2)
((sqrt(sum((x-mean(x))^2)/(length(x)-1))) * ## This is the square root of the sum of
                                            ## deviations controlling for sample size
(sqrt(sum((y-mean(y))^2)/(length(y)-1))))   ## This is the square root of the sum of
                                            ## deviations controlling for sample size

## Then you multiply both STANDARD deviations.

## Finally, if you divide the covariance by the multiplcation of both SD's, you get
## the correlation.


## Beta 1:

((sum(((x-mean(x)) * (y-mean(y)))/(length(x)-1)))/
((sqrt(sum((x-mean(x))^2)/(length(x)-1))) *
(sqrt(sum((y-mean(y))^2)/(length(y)-1))))) *
(((sqrt(sum((y-mean(y))^2)/(length(y)-1))))/
(sqrt(sum((x-mean(x))^2)/(length(x)-1))))

## Which is the same as: 
cor(x,y)*sd(y)/sd(x)
