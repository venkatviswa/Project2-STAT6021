# importing libraries
library(tidyverse)
library(car)
library(ROCR)
library(faraway)
library(MASS)
library(dplyr)
library(leaps)

# define train and test data frames
Data<-read.csv("kc_house_data.csv", sep=",", header=TRUE)
Data<-Data[, -c(1:2)]
set.seed(6021)
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F)
train<-Data[sample.data, ]
test<-Data[-sample.data, ]


# define first linear regression model with 7 predictors in data frame train_mlr
train_mlr <- train[, c('bedrooms', 'bathrooms', 'sqft_living', 'floors', 'condition', 'grade', 'sqft_above', 'price')]
result_mlr<-lm(price~., data=train_mlr)
summary(result_mlr)


# model diagnostics: check regression assumptions
par(mfrow=c(2,2))
plot(result_mlr)



# run Box Cox plot, assign ystar as log of price
boxcox(result_mlr, lambda=seq(-0.5,0.5,0.1))

#log transform y into second linear regression model
train_mlr$ystar<-log(train_mlr$price)
result_mlr_2<-lm(ystar~.-price, data=train_mlr)

##create diagnostic plots after y transformation
par(mfrow=c(2,2))
plot(result_mlr_2)


# regression assumptions are met! now let's check with ANOVA
# whether the model is useful (compare to intercept-only model)

regnull<-lm(ystar~1, data=train_mlr)
anova(regnull, result_mlr_2)



# success! here's the new regression model summary (write regression equation)
summary(result_mlr_2)



########### ########### ########### ########### ########### ###########

# we should note that there may be issues with multicollinearity
# try vif(result_mlr_2)

########### ########### ########### ########### ########### ###########


# now we try to simplify the model and remove some predictor variables
# consider a third linear regression model with only three predictors
reduced_mlr<-lm(ystar~bathrooms+sqft_living+grade, data=train_mlr)
summary(reduced_mlr)



# multicollinearity?
faraway::vif(reduced_mlr)



# now compare with the full model using ANOVA F test
anova(reduced_mlr, result_mlr_2)


# success! remember to write out the regression equation
# and to interpret the results from the summary for reduced_mlr

# Question: could we still remove bathrooms from this model? This 
#           predictor seems to be insignificant in the reduced model

two<-lm(ystar~sqft_living+grade, data=train_mlr)
summary(two)

# we carry out both ANOVA tests to see if it is better than both
# the full model and the three-predictor model
anova(two, result_mlr_2)

anova(two, reduced_mlr)


# so the two-predictor model is better than the full model, however 
# it seems to be just slightly worse than the three-predictor model

# now we can carry out the model selection with 
# result_mlr_2 (full model) and regnull (intercept-only model) 



# forward
step(regnull, scope=list(lower=regnull, upper=result_mlr_2), direction="forward")




# backward
step(result_mlr_2, scope=list(lower=regnull, upper=result_mlr_2), direction="backward")




# stepwise
step(regnull, scope=list(lower=regnull, upper=result_mlr_2), direction="both")



# all selections should support the six-predictor model 
# excluding only floors



# consider the regression subsets for the full model
allreg <- leaps::regsubsets(ystar~sqft_living + grade + condition + bedrooms + sqft_above + bathrooms + floors, data=train_mlr, nbest=1)
summary(allreg)


# now let's take a look at the influential observations, 
# high leverages, and outliers



#leverages
lev<-lm.influence(two)$hat
n<-dim(Data)[1]
p<-3
##identify
lev[lev>8*p/n]




# Outliers
##externally studentized residuals
ext.student.res<-rstudent(two)
##identify
ext.student.res[abs(ext.student.res)>3]




#influential observations in terms of DFFITS
DFFITS<-dffits(two)
DFFITS[abs(DFFITS)>8*sqrt(p/n)]




#influential observations in terms of DFBETAS
DFBETAS<-dfbetas(two)
abs(DFBETAS)>2/sqrt(n)



# Cook's distance
COOKS<-cooks.distance(two)
COOKS[COOKS>1]



# checking for any influential observations
influence_measures <- influence.measures(two)
high_influence <- influence_measures$infmat[, "hat"] > (2 * mean(influence_measures$infmat[, "hat"]))
train_no_influence <- train_mlr[!high_influence, ]
summary(train_no_influence)




# Re-fit the model without influential observations
two_updated <- update(two, data = train_no_influence)
summary(two_updated)






















































































































































































































































# in this section, we just consider what would happen 
# if we remove a few other predictors from the model



# let's try using a six predictor model by only removing bathrooms
# because it is the least significant predictor from the model
result_idea<-lm(ystar~.-price-bathrooms, data=train_mlr)
summary(result_idea)
vif(result_idea)

# compare this model with no bathrooms against the full model

anova(result_idea, result_mlr_2)


# very interesting! it appears the model would rather keep the
# bathrooms predictor rather than get rid of it
# although we still find a high VIF between the two square feet predictors


########### ########### ########### ########### ########### ###########



# given that there was multicollinearity with the two square feet
# predictors, we try only removing only the sqft_above from the model
# because sqft_living may give us a better idea of the actual
# area of each house
result_no_beef<-lm(ystar~.-price-sqft_above, data=train_mlr)
summary(result_no_beef)
vif(result_no_beef)


# now we have no VIF present in our model :)
# let's try this model versus the full model and see whether there
# is an improvement

anova(result_no_beef, result_mlr_2)

# it is indeed better! having no multicollinearity probably helped


########### ########### ########### ########### ########### ###########


# finally, we can try to got for a model that does not 
# contain neither bathrooms nor sqft_above
# we do this because we saw that bathrooms is still
# an insignificant predictor, which may suggest we can 
# remove it along with sqft_above


result_top_5<-lm(ystar~.-price-sqft_above-bathrooms, data=train_mlr)
summary(result_top_5)

vif(result_top_5)

# we see this one also has minimal VIF, so this is a good sign
# first, compare it with the original full model with 7 predictors


anova(result_top_5, result_mlr_2)

# it is better than the full model!
# now let's see if it is better than the model with no 
# sqft_above predictor in it (result_no_beef)


anova(result_top_5, result_no_beef)

# the data do not support getting rid of the bathrooms predictor
# from result_no_beef




