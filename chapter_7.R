rm(list=ls())
#' ## Chapter 6
library(mosaic)

#' A sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.
#' Data definition file:
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#' sales = S    Monthly sales revenue ($1000s)
#' price = P    A price index in $ for all products sold in a given month.
#' advert = A   Expenditure on advertising ($1000s)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
names(andy)

summary(andy)

#' Example 6.1
#' Testing joint hypotheses, the F-test
#' Equation 6.2 Unrestricted Model, all variables
unrestricted <- lm(sales~price+advert+I(advert^2), data=andy) 
summary(unrestricted) 

library(broom)
#' The anova table, sums of squares
anovaunrestricted  <- tidy(anova(unrestricted))
anovaunrestricted
sum(anovaunrestricted[1:3,]$sumsq) # SSR

#' Equation 6.3 Restricted model, remove advertising
#' The restriction is that, H0: b3 = b4 = 0, 5% level of significance
restricted <- lm(sales~price, data=andy) # restricted model, only price
summary(restricted)

#' The anova table, sums of squares
anovarestricted  <- tidy(anova(restricted))
anovarestricted
sum(anovarestricted[1,]$sumsq)

#' SSR contribution by the variables advert and advert2, I)
sum(anovaunrestricted[1:3,]$sumsq)-sum(anovarestricted[1,]$sumsq)
#' or, II)
sum(anovaunrestricted[2:3,]$sumsq)
#' Note that you can get this SSR by estimating both models, and doing SSRunrestricted-SSRrestricted.
#' Or just look at the unrestricted SSR

#' SSE unrestricted, has a smaller SSE
anovaunrestricted
sum(anovaunrestricted[4,]$sumsq)
#' SSE restricted, has a higher SSE
anovarestricted
sum(anovarestricted[2,]$sumsq)

#' F-test, equation 6.4 on the restriction
anova(restricted,unrestricted) 
#' Store the F-value
Ftest <- anova(restricted,unrestricted) 
str(Ftest)
Ftest$F[2] # the calculated F

#' The critical F-value
qf(0.95,2,71)

#' Draw the F-distribution
curve(df(x,2,71),0,10, col="aquamarine4", main="F-distribution with 2 and 71 df")
abline(h=0, col="grey")
abline(v=qf(0.95,2,71), col="red") # The critical F
abline(v=Ftest$F[2], col="blue") # the calculated F, and area to the right of the blue line is the p-value

anova(restricted,unrestricted) 
pf(Ftest$F[2],2,71, lower.tail = FALSE) # p-value
#' We reject H0, and conclude that advertising contribute in explaining the variance of sales

#' 6.1.1 Testing the significance of the model (compare with model that only has an intercept)
onlyinterc <- lm(sales~1, data=andy) # restricted model, equation 6.7
summary(onlyinterc)  

anova(onlyinterc,unrestricted) # F-test, equation 6.8
summary(unrestricted) # Same F-test as above
#' So comparing the two models by estimating both, is the same as the F-test in the summary of the unrestricted model

#' 6.1.2 The relationship between t- and F-tests
#' Example 6.4 when are t-tests and F-test equivalent?
#' When the F-test has 1 df!
lm(sales~price+advert+I(advert^2), data=andy) # Unrestricted model
lm(sales~advert+I(advert^2), data=andy) # Restricted model, remove price, 1 df difference

#' F-test anova(Restricted,Unrestricted) 
anova(lm(sales~advert+I(advert^2), data=andy), lm(sales~price+advert+I(advert^2), data=andy))
# save anova for later
F2 <- anova(lm(sales~advert+I(advert^2), data=andy),lm(sales~price+advert+I(advert^2), data=andy))

summary(unrestricted)
b <- coef(unrestricted) ; b # estimated coefficients
vcov(unrestricted) # variance-covariance matrix
diag(vcov(unrestricted)) # variance
b/sqrt(diag(vcov(unrestricted))) # t-values=estimated coefficients/se(b's)

t <- b/sqrt(diag(vcov(unrestricted))) # store t-values in vector
t[2] # t-value for price
t[2]^2 # t-value for price squared
F2$F # F-value equals the square of t-value

qt(0.975,71) # 5% critical t-value
qt(0.975,71)^2 # 5% critical t-value squared
qf(0.95,1,71) # is the same as 5% critical F-value with 1 df in numerator

#' It is also possible to start with the unrestricted model,
#' and then drop/remove the price variable.
#' This is the same as above.

#?drop1
m1 <- lm(sales~price+advert+I(advert^2), data=andy) # Unrestricted model, new name
summary(m1)
drop1(m1,~price, test="F")

#' 6.1.3 More general F-test
#' Unrestricted model
unrestricted <- lm(sales~price+advert+I(advert^2), data=andy) # Unrestricted model
summary(unrestricted)

#' Example 6.5 Testing optimal advertising; b3 + 2b4 advert0 = 1
#' 
#' Restricted model
#'  ---------------------------------------------
#' Is optimal advertising equal to $1900? p.267
#' ----------------------------------------------
#' H0: b3 + 2 b4 1.9 = 1, H1: b3 + 2 b4 1.9 != 1
#' Note that 2 x 1.9 = 3.8
#' Now "build" the restriction "into" the restricted regression model we estimate
#' If H0 is true, b3 = 1 - 3.8 b4, equation 6.12
restricted <- lm(I(sales-advert) ~ price + I(advert^2-3.8*advert), data=andy) # Restricted model
summary(restricted)

# Do an F-test
SSEu <- deviance(unrestricted) ; SSEu
SSEr <- deviance(restricted) ; SSEr
Fvalue <- ((SSEr-SSEu)/1)/(SSEu/71) ; Fvalue
#' The critical F-value
qf(0.95,1,71)
Fvalue < qf(0.95,1,71) # Calculated F < Critical F, hence we Keep H0

#' Do the test directly on the unrestricted model!
library(car)
library(multcomp)
library(tidyverse)

#' Same F-value as above
linearHypothesis(unrestricted, "advert + 3.8*I(advert^2) = 1") # car

#' One-tail test, create new variables under H0, example 6.6
andy <- andy %>% mutate(sales_advert=sales-advert,
               newadvert=advert^2-3.8*advert)

newrestricted <- lm(sales_advert~price+newadvert, data=andy) # Restricted model
summary(newrestricted) # same as above
summary(glht(newrestricted, linfct = c("newadvert <= 1"))) # H0: b3 + 3.8b4 <= 1
#' No evidence for H0, that optimal level of advertising is $1900

#' Joint Hypothesis, Example 6.7
#' J=2 Complex hypotheses
#' H0: b3 + 3.8 b4 = 1 and b1 + 6 b2 + 1.9 b3 + 3.61 b4 = 80
#' H0: optimal level of advertising is $1900 and a price of $6 and advertising of $1900 will give sales of $ 80000

#' As matrices, you may skip this:
Hypothesis <- matrix(c(0,0,1,3.8,1,6,1.9,3.61), 2, 4, byrow=TRUE)
RHS <- c(1,80)
Hypothesis ; RHS
colnames(Hypothesis) <- c("b1", "b2", "b3", "b4")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2")
Hypothesis ; RHS
linearHypothesis(unrestricted, Hypothesis, rhs=RHS)

#' Using variable names, notice the reversal of the order of the hypothesis
linearHypothesis(unrestricted, c("(Intercept)+6*price+1.9*advert+3.61*I(advert^2)=80",
                        "advert+3.8*I(advert^2)=1"))
#' At a 5% level the H0 is rejected

# -----------------------------------------------------------
#' 6.7 Not in Textbook, only one one-sided hypothesis
#' Unrestricted model
m1 <- lm(sales~price+advert+I(advert^2), data=andy) 
f <- makeFun(m1)
f(price = 6, advert=1.9)
coef(m1)[1]+coef(m1)[2]*6+coef(m1)[3]*1.9+coef(m1)[4]*1.9^2

#' Test if price=6 & advertising=1.9 ($1900)
#' equals 80 ($80000) in sales
library(rockchalk)

#' 95% Confidence interval
f(price = 6, advert=1.9, interval="confidence", level=0.95)
#' 80 is outside the confidence interval for the mean sales, at price=6!
plotCurves(m1, plotx = "advert", modx = "price", modxVals = 6, interval = "confidence")
abline(h=80, col="red")

linearHypothesis(m1, "6*price + 1.9*advert+3.61*I(advert^2) = 80")

# or
andy <- andy %>% mutate(advert2 = advert^2)
m2 <- lm(sales~price+advert+advert2, data=andy) # Unrestricted model
summary(glht(m2, linfct = c("6*price + 1.9*advert+3.61*advert2 <= 80"))) # One tail test
#' Keep Ho: predicted sales <= 80, sales are below 80 when price=6 & advert=1.9

# H0: b3 + 3.8b4 <= 1
summary(glht(m2, linfct = c("advert+3.8*advert2 <= 1"))) # One tail test of F test hypothesis above
# ------------------------------------------------------------

#' Optimal level of advertising, Example 6.8 A nonlinear hypothesis
car::deltaMethod(m2, "((1-advert)/(2*advert2))-1.9") # Non-linear combination of two parameters

m1
car::deltaMethod(m1, "((1-b3)/(2*b4))-1.9", parameterNames= paste("b", 1:4, sep=""))
m2
car::deltaMethod(m2, "((1-b3)/(2*b4))-1.9", parameterNames= paste("b", 1:4, sep=""))

car::deltaMethod(m2, "((1-x3)/(2*x4))-1.9", parameterNames= paste("x", 1:4, sep=""))
#' Value under H0 (0) is inside 95%, hence we keep H0


#' ## 6.2 The use of nonsample information
rm(list=ls())

# Obs:  30 annual observations from a single household
# 
# 1. q = litres of beer consumed
# 2. pb = Price of beer ($)
# 3. pl = price of other liquor ($)
# 4. pr = price of remaining goods and services (an index)
# 5. i = income ($)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/beer.rdata"))
names(beer)

summary(beer)

#' Unrestricted model eq. 6.17, log-log model
unrestricted <- lm(log(q)~log(pb)+log(pl)+log(pr)+log(i), data=beer)
summary(unrestricted)

b <- coef(unrestricted) ; b
b[2]+b[3]+b[4]+b[5] # sum is not equal to zero

#'  F-test on the restriction on the Unrestricted model
linearHypothesis(unrestricted, "log(pb)+log(pl)+log(pr)+log(i) = 0") # car function

#' Restricted model 6.20, under H0: b2+b3+b4+b5=0
#'  solve for pr, i.e., b4 = - b2 - b3 - b5 , and substitute into the restricted model 
restricted <- lm(log(q)~log(pb/pr)+log(pl/pr)+log(i/pr), data=beer)
summary(restricted)

c <- coef(restricted) ; c
-c[2]-c[3]-c[4] # use the restriction under H0 to recover the removed b4 coefficient for pr

#' Find its standard error
D <- c(0, 1, 1, 1)
lambda.se <- sqrt(t(D) %*% vcov(restricted) %*% D) # Std. Error of the removed coefficient from var-cov matrix
lambda.se

tvalue <- (-c[2]-c[3]-c[4])/lambda.se ; tvalue # t-value of the removed coefficient
#' Critical t
qt(0.975, df=df.residual(restricted), lower.tail=TRUE)
#' p-value
2*pt(tvalue, df=df.residual(restricted), lower.tail=F)

#' Using the deltaMethod from `car` on the Restriced model to recover the coefficient on b4 and its standard error
car::deltaMethod(restricted, "-b2-b3-b4", parameterNames= paste("b", 1:4, sep="")) 

#' Using fitModel (nls) from mosaic
#' note the name of the new function
?fitModel

f <- fitModel(log(q)~b1+b2*log(pb)+b3*log(pl)+b4*log(pr)+b5*log(i), data=beer)
summary(f)
summary(unrestricted)

#' Directly with the restriction specified, notice we substitute the restriction under H0 directly into the model,
#' as opposed to the example above where we "changed" the variables
g <- fitModel(log(q)~b1+b2*log(pb)+b3*log(pl)+(-b2-b3-b5)*log(pr)+b5*log(i), data=beer)
summary(g)
summary(restricted)

##########################################################

#' ## 6.3 Model Specification
rm(list=ls())
#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/edu_inc.rdata"))
names(edu_inc)

m1 <- lm(log(faminc)~hedu+wedu, data=edu_inc)
summary(m1)

m2 <- lm(log(faminc)~hedu, data=edu_inc) # excluding a relevant variable, introducing bias
summary(m2)

coef(m1)[3]*(cov(edu_inc$hedu,edu_inc$wedu)/var(edu_inc$hedu)) # bias 6.23

m3 <- lm(log(faminc)~hedu+wedu+kl6, data=edu_inc) 
summary(m3)

#' Irrelevant variables/model selection
m4 <- lm(log(faminc)~hedu+wedu+kl6+xtra_x5+xtra_x6, data=edu_inc)
summary(m4)

m5 <- lm(log(faminc)~hedu+kl6+xtra_x5+xtra_x6, data=edu_inc)
summary(m5)

library(stargazer)
stargazer(m1,m2,m3,m4,m5, type="text", intercept.bottom=FALSE,
          no.space=TRUE, omit.stat=c("LL","ser","f"))


# ---------------------------------- p.285
#' Akaike Information Criterion
AIC(m1)
#' Bayesian Information Criterion
BIC(m1)
# -----------------------------------

#' RESET Test
library(lmtest)

# Table 6.1 Bottom
resettest(m1, power=2, type="fitted") 
resettest(m1, power=2:3, type="fitted")

#' Manually
names(m1)
y <- m1$fitted.values
restricted <- lm(log(faminc)~hedu+wedu+I(y^2), data=edu_inc)
a <- summary(restricted)

(b <- coef(a)[, "t value"]) # or coef(a)[,3]
b[4]

#' Compare the two values
b[4]^2
resettest(m1, power=2, type="fitted")$statistic


#' ### Collinearity
rm(list=ls())
#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
names(andy)

#' When specifying a regression model with both x and $x^{2}$, the two variables will be highly correlated.
#' Also if you specify a model that has x, w, and the interaction xw, both x and w will be highly correlated with the product variable.
#' This is not a collinearity issue to be concerned about. The p-value for xw will not be affected by the multicollinearity. 
andy <- andy %>% mutate(advert2=advert^2)

m1 <- lm(sales~price+advert+advert2, data=andy) 
summary(m1)

cor(andy) # correlation
vif(m1) # package `car` function

plot(andy$advert,andy$advert2, xlim = c(0,4)) # Nonlinear relationship

vcov(m1) # Table 6.3

#' Base R prediction
newdf <- data.frame(price=6, advert=1.9, advert2=3.61)
newdf
predict(m1, newdata=newdf)
predict(m1, newdata=newdf, interval="predict") # default 95% PI
predict(m1, newdata=newdf, interval="confidence") # default 95% CI

#' mosaic::makeFun prediction, much better!
f <- makeFun(m1)
f(6,1.9,3.61)
f(6,1.9,3.61, interval="predict")
f(6,1.9,3.61, interval="confidence")

#' even shorter, only advert, notice one sweep of makeFun()
1.9^2
g <- makeFun(lm(sales~price+advert+I(advert^2), data=andy))
g(6,1.9)

rm(list=ls())
#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))
names(rice5)

#' Unrestricted model 1994 data
fit <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = filter(rice5, year==1994))
summary(fit)
confint(fit)
vif(fit)

#' Constant returns (restriction) to scale 1994 data
#' H0: b2 + b3 + b4 = 1, solve for area
fit2 <- lm(log(prod/area)~log(labor/area)+log(fert/area), data = filter(rice5, year==1994))
summary(fit2)
vif(fit2)

#' Recovering area parameter in constant returns model, from restriction under H0
deltaMethod(fit2, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' Estimate this model directly by using fitModel() function
g <- fitModel(log(prod) ~ b1 + (1-b3-b4)*log(area) + b3*log(labor) + b4*log(fert), data = filter(rice5, year==1994))
summary(g)
summary(fit2)

#' Data from 1993 and 1994
fit3 <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = filter(rice5, year %in% c(1993,1994)))
summary(fit3)
confint(fit3)
vif(fit3)

#' Constant returns to scale 1993 and 1994
fit4 <- lm(log(prod/area)~log(labor/area)+log(fert/area), data = filter(rice5, year %in% c(1993,1994)))
summary(fit4)
vif(fit4)
#' Recovering area parameter in constant returns model
deltaMethod(fit4, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

# p.295
#' Using fitModel (nls) from mosaic
rm(list=ls())
#' Example 6.19
#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/nlls.rdata"))
names(nlls)

fit <- fitModel(y~b1*x1+b1^2*x2, data=nlls)
summary(fit)

# locally minimum
summary(fitModel(y~b1*x1+b1^2*x2, data=nlls, start=list(b1=-2)))

# Make figure 6.1
sse_fit <- function(b1) { sum((nlls$y-b1*nlls$x1-b1^2*nlls$x2)^2) }

sse_fit(-3)
betas <- seq(-3,3, length.out = 100) # a vector of betas

map(betas, sse_fit)
ssevals <- unlist(map(betas, sse_fit))

plot(betas,ssevals, type = "l", xlab="Numeric value of beta", ylab="Sum of Squared Errors")

rm(list=ls())

#' Read the data
# Example 6.20
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/steel.rdata"))
names(steel)

# very sensitive to starting values
fit <- fitModel(eaf~a/(1+exp(-b-d*t)), data=steel, start=list(a=0.8,b=-1,d=0.05))
summary(fit)

fit(t=1)
xyplot(eaf~t, data = steel, xlim = c(0,100), ylim = c(0,1))
plotFun(fit(t) ~ t, add=TRUE) # the true figure 6.2

# A bit more flexible: nls() function
?nls

y <- steel$eaf 
t <- steel$t
start <- list(b1=0.8,b2=-1,b3=0.05)

fit2 <- nls(y~b1/(1+exp(-b2-b3*t)), start=start)
summary(fit2)
resid(fit2)
coef(fit2)

#' turning point
car::deltaMethod(fit2, "b1/2", parameterNames= paste("b", 1:3, sep="")) 
#' turning point at time t
car::deltaMethod(fit2, "-b2/b3", parameterNames= paste("b", 1:3, sep="")) 
