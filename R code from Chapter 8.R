
#' Heteroskedasticity

rm(list=ls())

library(tidyverse)
library(mosaic)
library(broom)

# Data definition file
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")
# Load the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))
head(food)

# Example 8.1 Heteroskedasticity in the food expenditure model
fit <- lm(food_exp~income, data = food)
summary(fit)

# Figure 8.2, ggplot
fit %>% augment() %>% select(.resid, income) %>% ggplot(aes(x=income,y=abs(.resid))) + geom_point(col="darkgreen") +
  ggtitle("Figure 8.2: Absolute value of food expenditure residuals vs. income") +
  xlab("Weekly household income") + ylab(expression(abs(phantom(x)*hat(e)[i]*phantom(x)))) +
  theme_classic()

# Figure 8.2, base R
plot(food$income,abs(resid(fit)), pch=19, col="darkgreen",
     main="Figure 8.2: Absolute value of food expenditure residuals vs. income",
     xlab="Weekly household income", ylab=expression(abs(phantom(x)*hat(e)[i]*phantom(x))))

# Figure 8.3, ggplot
fit %>% augment() %>% select(.resid,income) %>% ggplot(aes(x=income,y=.resid)) + geom_point() +
  ggtitle("Figure 8.3: Food expenditure residuals vs. income") +
  xlab("Weekly household income") + ylab(expression(hat(e)[i])) +
  geom_hline(yintercept=0, col="red")

# Figure 8.3, base R
plot(food$income,resid(fit))
abline(h=0, col="red")

# If we save the broom::augment() as data
fit.metrics <- augment(fit)

# We could try to make a better picture of the heteroscedasticity.
# The residuals are the length of the red lines.
ggplot(fit.metrics, aes(x=income, y=food_exp)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = income, yend = .fitted), color = "red", size = 0.3)

head(fit.metrics)
# .fitted: fitted values
# .resid: residual or errors
# .hat: hat values, used to detect high-leverage points (or extreme values in the predictors x variables)
# .std.resid: standardized residuals, which is the residuals divided by their standard errors. Used to detect outliers (or extreme values in the outcome y variable)
# .cooksd: Cook's distance, used to detect influential values, which can be an outlier or a high leverage point

#' Regression Assumption: Homogeneity of variance
#' This assumption can be checked by examining the scale-location plot, also known as the spread-location plot.
plot(fit, 3)

#' This plot shows if residuals are spread equally along the ranges of predictors.
#' It's good if you see a horizontal line with equally spread points.
#' In this example, this is not the case.

#' It can be seen that the variability (variances) of the residual points increases with the value
#' of the fitted outcome variable, suggesting non-constant variances in the residuals errors (or heteroscedasticity).



# Example 8.2 Robust standard errors
# Correct Variance Covariance matrix for Heteroskedasticity
library(sandwich)

# These are the OLS (incorrect) standard errors
sqrt(diag(vcov(fit)))

# using pipes, se's
fit %>% vcov() %>% diag() %>% sqrt()

# These are the White HCE corrected standard errors, POE5 p. 374
sqrt(diag(vcovHC(fit, type = c("HC1")))) 

# using pipes, robust se's
fit %>% vcovHC(type = c("HC1")) %>% diag() %>% sqrt()

# OLS estimates and standard errors
library(lmtest)
coeftest(fit)

# OLS estimates and White HCE standard errors
coeftest(fit, vcov=vcovHC, type = c("HC1") )

# Same procedure, but using the car package.
library(car)
sqrt(diag(hccm(fit, type = c("hc1"))))

# Now we can also create a CI using car::Confint()
Confint(fit, vcov.=hccm(fit, type = c("hc1")))

# You can also test hypothesis using the corrected VCOV matrix
linearHypothesis(fit, c("(Intercept) = 0"), 
                 vcov = hccm(fit, type = "hc1"))


# Chapter 8.4 GLS, known form of variance, aka. Weighted Least Squares (WLS)
# We assume that var(e|x)= sigma^2 * x, hence the weight is 1/x since 1/x * sigma^2 * x = sigma^2
fit.gls <- lm(food_exp~income, weights=I(1/income), data=food)
summary(fit.gls)
confint(fit.gls)

# Long way WLS, equation 8.13, p. 376
# We never do this in practice, we use the weights= option!
# We still assume that var(e|x)= sigma^2 * x
# but notice the weights in this manual procedure is 1/sqrt(x) since var(1/sqrt(x))=1/sqrt(x)^2=1/x
fit.gls.alt = lm(I(food_exp/sqrt(income))~-1+I(1/sqrt(income))+I(income/sqrt(income)), data=food)
summary(fit.gls.alt)

# Compare coefficents between models
library(stargazer)
stargazer(fit, fit.gls, fit.gls.alt, type="text")

# Figure 8.4, p. 378
ols_resid <- augment(fit) %>% select(.resid)
gls_resid <- resid(fit.gls.alt)

plot(food$income,ols_resid$.resid, pch=16, col="red")
points(food$income,gls_resid, pch=16, col="black")
legend(29, 150, legend=c("OLS residuals", "GLS residuals"),
       col=c("red", "black"), pch=c(16,16))



# Example 8.4 Multiplicative Heteroscedasticity in the Food Expenditure Model
# Estimating the variance function, p. 382

# Note that we save the log of the squared residual in the fit.metrics data,
# since it already contains the residual and variables.
fit.metrics <- fit.metrics %>% mutate(e2=log(.resid^2))

fit2 <- lm(e2~log(income), data = fit.metrics)
summary(fit2)

# Calculating the correction h()
sigma2 <- exp(coef(fit2)[2]*log(food$income))
# sigma2 <- exp(predict(fit2)) # easier

# Then do Weighted Least Squares model, eqtn. 8.21
fit.gls.var <- lm(food_exp~income, weights=I(1/sigma2), data=food)
summary(fit.gls.var)



#' Detecting Heteroscedasticity
library(lmtest)

# Example 8.7, p. 385
# Goldfeld-Quandt test against heteroskedasticity
#?gqtest
gqtest(fit)

# Breusch-Pagan bp(model), p. 387
#?bptest

# The H0 is homoscedastic errors
bptest(fit, studentize = FALSE) # B-P or LM test
ncvTest(fit) # package car, same test

# White test
# second order
bptest(lm(food_exp~income+I(income^2), data=food), studentize=F)
# second order and third order
bptest(lm(food_exp~income+I(income^2)+I(income^3), data=food), studentize=F)

# some other tests
browseURL("https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html")
browseURL("https://cran.r-project.org/web/packages/olsrr/")

########### end


# Not run, not SOK-3020 curriculum
##################################
library(brms)

# normal errors
fit.1.brms <- brm(bf(food_exp ~ income, sigma~income), data = food)
fit.1.brms

# robust, t-distributed errors
fit.2.brms <- brm(bf(food_exp ~ income, sigma~income), family=student, data = food)
fit.2.brms

# multiplicative, normal errors
fit.3.brms <- brm(bf(food_exp ~ income, sigma~log(income)), data = food)
fit.3.brms

# model 2, robust with t-distribution seems best
fit.1.brms <- add_criterion(fit.1.brms, c("waic","loo"))
fit.2.brms <- add_criterion(fit.2.brms, c("waic","loo"))
fit.3.brms <- add_criterion(fit.3.brms, c("waic","loo"))

loo_compare(fit.1.brms, fit.2.brms, fit.3.brms, criterion = "loo")
loo_compare(fit.1.brms, fit.2.brms, fit.3.brms, criterion = "waic")

model_weights(fit.1.brms, fit.2.brms, fit.3.brms, weights = "loo")
model_weights(fit.1.brms, fit.2.brms, fit.3.brms, weights = "waic")

#model_weights(fit.1.brms, fit.2.brms, fit.3.brms, weights = "kfold")
# fit.1.brms fit.2.brms fit.3.brms 
# 0.3465233  0.0256193  0.6278574 

model_weights(fit.1.brms, fit.2.brms, fit.3.brms, weights = "stacking")

# Likelihood approach
#install.packages("hett")
library(hett)

fit.2.hett <- tlm(food_exp ~ income, ~ income,
                  data = food,
                  start = list(dof = 3), estDof = TRUE)
summary(fit.2.hett)

fit.3.hett <- tlm(food_exp ~ income, ~ log(income),
                  data = food,
                  start = list(dof = 3), estDof = TRUE)
summary(fit.3.hett)
# model 2, robust with t-distribution seems best