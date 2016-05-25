library(arm)

## Let's read the dataset
politeness=
    read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

## Explore a little
head(politeness)
dim(politeness)

## Have a look at the variables
str(politeness)
## So the variable subject specifices each respondent(which were followed up longitudinally so their responses are not independent)
## Additionally, each respondent was assigned a specific scenario on which to have an attitude. So, for example, 
## subject F1 was assigned scenario 1 in some instances and scenario 2 in other instances, interviews are nested into subject
## and subjects are nested into scenarios

which(is.na(politeness$frequency))
## There is a missing value in row 39 of frequency(our dv)

# The difference in politeness level is represented in the column called “attitude”.
# In that column, “pol” stands for polite and “inf” for informal. Sex is represented
# as “F” and “M” in the column “gender”. The dependent measure is “frequency”,
# which is the voice pitch measured in Hertz (Hz). To remind you, higher values
# mean higher pitch.

boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)

lmer(frequency ~ attitude, data=politeness)
## Remember that this is a mixed model, so you should have some fixed and random part! Here you only have a fixed part.

politeness.model = lmer(frequency ~ attitude + (1|subject), data=politeness)
display(politeness.model)
## Let's interpret some results from this.

## First, the coefficients.
## Attitudepol is a dummy in which 1 == polite and 0 == informal
## The intercept here is the average frequency for subject in which attitud pol is 0(informal)
## attitudepol is then the average frequency difference between polite and informal.
## so subjects assigned to polite scenarios were -19.38 less frequent than informal.

## You can also interpret this as looking at the difference between polite and informal controlling for subject

## Second, the error terms.
## The subject(intercept) standard deviation is how much each subject mean frequency varies per subject.
sd(ranef(politeness.model)[[1]][,1])
## Confirm it yourself
## In other words, because subjects are our second level variable, this is the BETWEEN subject variability.

## The residual is the left over unexplained variation, which if it nos between subject, it MUST to be the within subject.
## So now we have the within and between subject variability.

## You can also interpret it as this:
# “Residual” which stands for the variability that’s not due to
# subject. This is our “ε” again, the “random” deviations from the predicted values
# that are not due to subjects. Here, this reflects the fact that each and
# every utterance has some factors that affect pitch that are outside of the purview
# of our experiment.

63.10/(63.10+29.17)
## The ICC is straightforward to calculate: divide the between subject variability by the overall variability:
## 68% of the variability of politeness is attributable to differences between subjects after accounting for attitude

## Since we specificed at the beginning that subjects were nested into scenarios, we have to also allow the scenario
## to vary by subject

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
display(politeness.model)

## Let's interpret again
# The intercept is the mean frequency for attitudepol == 0, which is informal attitude.
# Do not get confused because you have two varying intercepts. If you have two random
# intercepts then the random effects will be how each subject and scenario differ from the
# mean frequency of informal attitude. That is why we need to almost always center our IV's
# to have meaningful intercepts.

ranef(politeness.model) ## As you can see, some scenarios have low or higher mean frequency as well as the subject.

## One question though: are these scenarion and subject deviances for when scenario and subject equal informal attitude or
## for the overall mean scenario or mean subject.
# I have a hunch that it is from the mean scenario when informal attitude, because otherwise it doesn't make much sense
# to compare the overall scenario mean(for exmaple) to the overall mean when informal attitude.

## Let's interpret random effects
display(politeness.model)

# We can see that most of the variance is due to differences between subjects followed by differences within
# subjects. Finally, there is some variability between scenarios, although not substantial compared to between subject variability

# As showed earlier, some of the difference between polite and formal change dratistically between gender. Let's include it in the
# model

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)
display(politeness.model)

# Let's interpret.
# The intercept is the mean frequency for informal attitude and women: 256.85
# attitudepol is the difference for formal attitude. So formal women have 256.85-19.72= 237
# genderM is the difference between men and informal women. So informal men have a mean frequency of 256.85-108.52=148
# Finally, since we have mean frequency of informal men, we simply subtract the formal factor 148-19.72=128

## Random effects of model without gender
# Error terms:
# Groups      Name        Std.Dev.
# scenario (Intercept)     14.80  
# subject  (Intercept)     63.36  
# Residual                 25.42

# Let's compare random effects of both models.
# The scenario intercept variation changed insignificantly.
# The subject variance decreased SUBSTANTIALLY when gender is included, meaning that most of the between subject variance
# was due to gender differences
# Finally, the residual variance is virtually unchanged when adding gender

# Let's cover the analogous procedure of the F test in multilevel.
# Similar to the lm function, here you use the anova() function but it differs in the procedure in which
# tests for improvements.
politeness.model.1 = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness, REML = FALSE)
politeness.model.2 = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness, REML = FALSE)
anova(politeness.model.1, politeness.model.2)
##### REMEMBER TO PUT REML=FALSE FOR EACH MODEL YOU'RE INTERESTED IN TESTING. ####

# Here the addition of gender significantly improves the model

# So far we haven't touched on the usefulness of varying slopes.
coef(politeness.model)
# We can see that the intercept for each subject and scenarios varies but that the slope
# of the individual coefficients is fixed for every subject and every scenario. This is theoretically
# quite unlikely because we would expect for it to change.

politeness.model = lmer(frequency ~ attitude + gender +
                        (1+attitude|subject) +
                        (1+attitude|scenario),
                        data=politeness,
                        REML=FALSE)
## So the only thing we added on this model was that the attitude slope varies by subject and scenario.
coef(politeness.model)
# Look at how attitude differs per subject and scenario
# Note, however, that it’s always negative and that many of the values are quite similar to each
# other. This means that despite individual variation, there is also consistency in
# how politeness affects the voice: for all of our speakers, the voice tends to go
# down when speaking politely, but for some people it goes down slightly more so
# than for others.

####### Assumptions in Multilevel #########

# You also have to worry about collinearity and influential data points. And you have to worry about
# homoscedasticity (and potentially about lack of normality). But you don’t have to
# learn much new stuff. The way you check these assumptions in R is exactly the
# same as in the case of the linear model, say, by creating a residual plot, a
# histogram of the residuals or a Q-Q plot.

# Independence, being the most important assumption, requires a special word: One
# of the main reasons we moved to mixed models rather than just working with
# linear models was to resolve non-independencies in our data. However, mixed
# models can still violate independence … if you’re missing important fixed or
# random effects. So, for example, if we analyzed our data with a model that didn’t
# include the random effect “subject”, then our model would not “know” that there
# are multiple responses per subject.

# This is the same as when you're missing a hierarchichal clustering variable. Imagine we are
# studying(just like Eric, political science friend) party electoral turnout and we only have voters
# clustered into parties. We are still biased because parties would mostly be clustered into provinces
# So a failure to include random effects for province will still introduce bias.

# Finally, You will find that the function
# dfbeta() that we used in the context of linear models doesn’t work for mixed
# models. If you worry about influential points, you can check out the package
# influence.ME
