#' deltaMethod()

#' In this R code we show how to use
#' the deltaMethod() function from the car package 
#' to construct an interval estimate for the expected value 
#' of the dependent variable and an elasticity. In addition we
#' can perform hypothesis testing using the function

rm(list=ls())

library(mosaic)

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

str(food)

#' Estimating the parameters of the simple linear regression model in Chapter 2
fit <- lm(food_exp~income, data=food)
fit

#' A more general output from the model is given by
summary(fit)

#' The fitted regression
plotModel(fit) 

# interval estimation 
f <- makeFun(fit)
f(20, interval="confidence", level=0.95)

# Hypothesis testing 
# E(food_exp | income=20) = b1+b2*20 
# H0: b1+b2*20 <= 250, H1: b1+b2*20 > 250 
# Rewrite: H0: b1+b2*20 -250 <= 0, H1: b1+b2*20 -250 > 0 

library(car)
#?deltaMethod

deltaMethod(fit, "Intercept+20*income-250") 

# more convenient than names, use parameter names
deltaMethod(fit, "b1+20*b2-250", parameterNames=paste("b", 1:2, sep="")) 

paste("b", 1:2, sep="")
paste("a", 1:2, sep="")

# save as object
dmt <- deltaMethod(fit, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 
names(dmt)

# t-value
dmt$Estimate/dmt$SE

# critical t, alpha=0.05
alpha <- 0.05
qt(1-alpha, df.residual(fit))  #df = N-2 degree of freedom 

# compare calculated to critical
dmt$Estimate/dmt$SE > qt(0.95, 38)
#conclusion: reject H0


#  Estimate the elasticity of expected food exp.
els <- function(x,y) {coef(fit)[2]*x/y}

# The sample mean of income and food_exp
mx <- mean(food$income)
my <- mean(food$food_exp)

els(mx, my)

# A 95% CI
deltaMethod(fit, 
            "b2*mx/my", 
            parameterNames=paste("b", 1:2, sep=""),
            level=0.95)

# Hypothesis testing 
# Test the null hypothesis that the elasticity, evaluated
# at the sample mean of income and food_exp is 1 against the alt. that the 
# elast is not one.
# H0:b2*mx/my=1 vs H1:b2*mx/my != 1
# Rewrite: H0:b2*mx/my-1=0 vs H1:b2*mx/my-1 != 0

deltaMethod(fit, 
            "b2*mx/my-1", 
            parameterNames=paste("b", 1:2, sep=""),
            level=0.95)

dmt2 <- deltaMethod(fit, 
                    "b2*mx/my-1", 
                    parameterNames=paste("b", 1:2, sep=""),
                    level=0.95) 
# t-value
dmt2$Estimate/dmt2$SE

#Two tail test 
# critical t, alpha=0.05
# 2.5% critical value 
alpha <- 0.05

qt(alpha/2, df.residual(fit))   #df = N-2 degree of freedom 

#97.5% crtitical value 
qt(1-alpha/2, df.residual(fit))  

# compare the t-calculated with the critical values 
dmt2$Estimate/dmt2$SE < qt(alpha/2, df.residual(fit)) # Hence reject Ho
dmt2$Estimate/dmt2$SE > qt(1-alpha/2, df.residual(fit))

#conclusion: reject H0

# Marginal effects
browseURL("https://marginaleffects.com")
library(marginaleffects)

# slope
marginaleffects(fit, newdata="mean", slope = "dydx")

# elasticity at means
marginaleffects(fit, newdata="mean", slope = "eyex")
marginaleffects(fit, newdata="mean", slope = "eyex", conf_level=0.9)

# also a delta method function
deltamethod(fit, "b2*mx/my=0")
deltamethod(fit, "income*mx/my=0")
