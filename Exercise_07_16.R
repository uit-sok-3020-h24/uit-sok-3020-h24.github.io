
# The R code from the class earlier today 

# Exercise: Q 7_16

rm(list = ls())
library(mosaic)

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
