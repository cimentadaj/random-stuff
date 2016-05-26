## It is common for researchers to choose one predictor over another simply because one predictor is significant
## and the other is not. However, perhaps the DIFFERENCE between both predictors coefficient is not significant,
## hence the title: The difference between significant and non-significant results is not statistically significant.

## Let's explore an example case.


## Let's suppose we have this regression models and we're interested in the hypothesis that disp is a better
## predictor than hp and qsec
options(scipen=999)
summary(lm(wt ~ disp + hp + drat + qsec, mtcars))$coef
## From the results we can actualy see that disp is significant and hp is not. We therefore would conclude that 
## hp is a significant predictor and hp is not.

## We could confirm that by estimating if the coefficient is two standard errors above 0
# literally, the formula is abs(regression coefficient) - 2*standard error

abs(0.006293291) - 2 * 0.001237807 ## disp is - confirmed
abs(0.003536668) - 2 * 0.002265374 ## hp is not - confirmed

## but are these two actually significantly different?

abs(0.002756623) - 2 * -0.001027567

## yes they are! however, they are quite close of not being as it is only by 0.004
# so we can conclude that disp it not only significant predictor but that it is significantly different from hp

## Let's exnted it to the qsec predictor, which has a much stronger coefficient than hp

## Difference between coef and se of both predictors
c(0.006293291,0.0012378070) - c(0.193099658 ,0.060608362)
abs(-0.18680637) - 2 * -0.05937055 ## it is highly significant! the difference between disp and qsec is above 0 by .30
## if we compared that to the 0.004 difference between disp and hp then we can have confidence that qsec is indeed much
## more significantly different
