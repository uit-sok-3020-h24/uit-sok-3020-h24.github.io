
# Q 7.16

rm(list=ls())
library(mosaic)
library(broom)
library(car)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5mw.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5mw_small.rdata"))

head(cps5mw_small)

#' (a). 
#  Estimate the linear regression model. 
# hrswork ~ b1 + b2*wage + b3*educ + b4nchild +e 

fit1 <- lm(hrswork ~ wage+educ+nchild, data = cps5mw_small)
summary(fit1)

#Interprate the coeff of nchild

#' The estimated coefficient of NCHILD is negative and significant, suggesting
#'  that as the number of children increases we predict the expected number of hours
#'  worked to fall by 0.4501, holding other factors fixed. And it is statistically signi.

f = makeFun(fit1)

# Expected hours worked by a married women whose wage is $20 per hour, who has 16 years of edu,
# who has no children.
f(wage = 20, educ = 16, nchild = 0)

# women with one child
f(wage = 20, educ = 16, nchild = 1)

# 2 children 
# women with one child 
f(wage = 20, educ = 16, nchild = 2)

#three children 
# women with one child 
f(wage = 20, educ = 16, nchild = 3)

# Q: How much does the expected number of hours change with each additional child?
# women with one child 
f(wage = 20, educ = 16, nchild = 1)-f(wage = 20, educ = 16, nchild = 0)
f(wage = 20, educ = 16, nchild = 2)-f(wage = 20, educ = 16, nchild = 1)
f(wage = 20, educ = 16, nchild = 3)-f(wage = 20, educ = 16, nchild = 2)

#' (b) 
#' Indicator variables: 
#' postgrad = 1, if educ >16, 0 otherwise 
#' college = 1, if educ = 16, 0 otherwise
#' somecollege = 1 if 12 < educ < 16, 0 otherwise

cps5mw_small <- cps5mw_small %>% 
  mutate(postgrad = ifelse(educ>16,1,0),
         college = ifelse(educ==16,1,0),
         somecollege = ifelse(educ>12 & educ <16,1,0))

#' Estimate expected hours worked, hrswork replacing educ with these three indicator variables
fit2 <- lm(hrswork ~ wage+postgrad+college+somecollege+nchild, data = cps5mw_small)
summary(fit2)

#' In this model the marginal effect of education is not constant. The effects of some college,
# college and post-graduate education are quite different.

#' The expected number of hours worked for women earning $20 and with no children, and with
#' 12 years of education is 39.97. 

f= makeFun(fit2)
f(wage = 20,nchild = 0,somecollege = 0,college=0, postgrad=0)

#'  with 13, 14, or 15 years of education we estimate
#' the expected number of hours worked to be 39.88. 
f(wage = 20,nchild = 0,somecollege = 1,college=0, postgrad=0)

#' For women with 16 years of education this rises to 42.16 hours, 
f(wage = 20,nchild = 0,somecollege = 0,college=1, postgrad=0)

#' and for women with 17 years of education it rises to 44.07 hours.
f(wage = 20,nchild = 0,somecollege = 0,college=0, postgrad=1)

# conclusion: The marginal effect of education is not constant!

#' (c).
#' Define indicator variables: 
#' onekid = 1 if Nchild=1, 0 otherwise
#' Twokids = 1, if Nchild=2, 0 otherwise
#' Morekids = 1, if Nchild >2, 0 0therwise

#' Q. Estimate the the hrswork model, but replace Nchild by these three indicator variables

cps5mw_small <- cps5mw_small %>% 
  mutate(onekid = ifelse(nchild==1,1,0),
         twokids = ifelse(nchild==2,1,0),
         morekids = ifelse(nchild >2,1,0))

fit3 <- lm(hrswork ~ wage+educ+onekid+twokids+morekids, data = cps5mw_small)
summary(fit3)

# Interpret the estimated coeffs. of the three indicator variables


# Then go to explain the PPT on: Applying Indicator Variables (slide number = 14)






# 7. 17

rm(list=ls())
library(mosaic)
library(broom)
library(car)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/bweight_small.def")

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/bweight_small.rdata"))

head(bweight_small)



#' a). 

t.test(bweight ~mbsmoke, data = bweight_small)

#' Since the p-value =7.409e-11 is less than alpha = 5%, 
#' we reject H0 that the birthweights of smoking and nonsmoking 
#' mothers are the same. 

#' b).

m1 <- lm(bweight ~ mbsmoke, data = bweight_small)
summary(m1)

#' It is not wise to consider this regression
#' a basis for estimating an average treatment effect.
#'  It would be a consistent estimator only if
#' women were randomly assigned to smoking and non-smoking groups.

library(multcomp)
# H0: b2 >=0 vs H1: b2 <0
summary(glht(m1, linfct = c("mbsmoke >=0")))

#' The one tail critical value is 
qt(0.05,length(bweight_small$mbsmoke))
#' We conclude that women who smoke have significantly 
#' lower birthweight babies, with the estimated reduction being 288 grams, 
#' or about 10.16 ounces. 

# c).
names(bweight_small)

m2 <- lm(bweight ~ mbsmoke + mmarried + mage + prenatal1 + fbaby , data = bweight_small)
summary(m2)
#' The coefficient on the indicator that the
#' mother smokes increases in magnitude to -229.9757, with t = -5.13.
#' This is a difference of 58.2089 grams, or about 2 ounces.
#' Of the other variables only MMARREID is significant. It has a positive
#' sign suggesting that mothers who are married have higher birthweight babies. 
#' This is plausible.

# The joint F-test of these control variables:
library(car)
linearHypothesis(m2, c("mmarried=0","mage=0","prenatal1=0", "fbaby=0"))

#' The p-value is less than alpha=5%.
#' So collectively we can say at least one of the 
#' coefficients helps explain the outcome.


#' d). 

# Regression for mbsmoke =1
bweight_smoke <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby , data = filter(bweight_small,mbsmoke ==1))
summary(bweight_smoke)

# Regression for mbsmoke =0 
bweight_not_smoke <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby , data = filter(bweight_small,mbsmoke ==0))
summary(bweight_not_smoke)


# Testing the equivalence of two regressions, chow test

#' Create interaction variables between all the explanatory variables and the indicator variable mbsmoke
bweight_small <- bweight_small %>% mutate(mmarried.mbsmoke = mbsmoke*mmarried,
                                          mage.mbsmoke = mbsmoke*mage,
                                          prenatal1.mbsmoke = mbsmoke*prenatal1,
                                          fbaby.mbsmoke = mbsmoke*fbaby)
fit <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby + mbsmoke + mmarried.mbsmoke + mage.mbsmoke + prenatal1.mbsmoke + fbaby.mbsmoke , data = bweight_small)
summary(fit)

joint.hyp <- c("mbsmoke=0",
               "mmarried.mbsmoke=0",
               "mage.mbsmoke=0", 
               "prenatal1.mbsmoke=0",
               "fbaby.mbsmoke=0")
library(car)
linearHypothesis(fit, joint.hyp)

# Critical value 
qf(0.95,5,1190)

#' Thus, we reject the null hypothesis. And conclude that there is 
#' some difference in the coefficients of the two equations 



# 7.27

rm(list=ls())
library(mosaic)
library(broom)
library(car)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def")

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

head(collegetown)

# a)

m1 <- lm(log(price) ~ log(sqft) + close, data = collegetown)
summary(m1)


#b).

m2 <- lm(log(price) ~ log(sqft) + I(close*log(sqft)), data = collegetown)
summary(m2)

# write the separate equation when close =0 and close = 1
#' The coef of log(sqft) captures the elasticity of house not close to the univ.,
#' whereas the coef of the interaction captures the difference in the elasticity
#' when the house is close and not close to the university. 
#' 

#' c). 
m3 <- lm(log(price) ~ log(sqft) + close + I(close*log(sqft)), data = collegetown)
summary(m3)


#' d)
f = makeFun(m3)

sigma2 <- exp(0.5*deviance(m3)/m3$df.residual)

# When close =1
#natural predictor 
f(sqft=25,close = 1)
#corrected predictor 
f(sqft=25,close = 1)*sigma2

#when close=0

f(sqft=25,close = 0)
f(sqft=25,close = 0)*sigma2

#The difference
(f(sqft=25,close = 1)-f(sqft=25,close = 0))*sigma2


# e). 
names(collegetown)
m4 <- lm(log(price) ~ log(sqft) + close + I(close*log(sqft))+fireplace +twostory + occupied, data = collegetown)
summary(m4)

#f)

collegetown <- collegetown %>% mutate(sqft.close=log(sqft)*close,
                                      fireplace.close = fireplace*close,
                                      twostory.close = twostory*close,
                                      occupied.close =occupied*close)

m4 <- lm(log(price) ~ log(sqft)+fireplace +twostory + occupied+close+sqft.close+fireplace.close+twostory.close+occupied.close, data = collegetown)
summary(m4)

library(car)
joint.hyp <- c("close=0",
               "sqft.close =0",
               "fireplace.close=0",
               "twostory.close=0",
               "occupied.close=0")
linearHypothesis(m4,joint.hyp)

#'The p-value is 0.0000. We conclude that there
#'is a significant difference between the houses close to the university and those not close even
#'at the .001 level of significance.






