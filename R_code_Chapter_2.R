#' R. Carter Hill, William E. Griffiths and Guay C. Lim,  
#' Principles of Econometrics, Fifth Edition, Wiley, 2018.

#' Clearing the workspace
rm(list=ls())

#' Loading the mosaic package for data analysis and visualization
library(mosaic)

#' Data set on food expenditure and weekly income from a random sample of 40 households.
#' Data definition file: <http://www.principlesofeconometrics.com/poe5/data/def/food.def>
#' Opening the data definition URL in the default web browser
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

#' Loading the food dataset from the provided URL
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

#' Displaying the first 3 rows of the food dataset
head(food, 3)
#' Displaying the last 4 rows of the food dataset
tail(food, 4)

#' The summary command reports some descriptive statistics (Table 2.1, p. 59)
#' Getting a summary of the food dataset
summary(food)

#' To be able to work with the variable names directly we use the *$* assignment
#' These commands replicate the descriptive statistics 
#' Accessing the food_exp column of the food dataset
food$food_exp
#' Calculating and displaying various descriptive statistics for the income column
mean(food$income)   # calculates the mean
median(food$income) # calculates the median
max(food$income)    # maximum value
min(food$income)    # minimum value
sd(food$income)     # calculates the standard deviation

#' Calculating a range of statistics for the income column in one go
favstats(~income, data=food) # one sweep
#' Storing the statistics in a variable called stats
stats <- favstats(~income, data=food)
#' Accessing and displaying specific statistics from the stats variable
stats$n
stats$mean
stats$median
favstats(~income, data=food)$mean
favstats(~income, data=food)$sd

#' Displaying the structure of the stats variable
str(stats)

#' Figure 2.6
#' Plotting food expenditure against income
xyplot(food_exp ~ income, data=food)

#' Plotting food expenditure against income using ggplot2
food %>% ggplot(aes(x = income, y = food_exp)) +
  geom_point(col="red") + labs(title = "food expenditure against income")

#' Estimating the parameters of the simple linear regression model in Chapter 2
#' Displaying the column names of the food dataset
names(food)
#' Performing a linear regression of food expenditure on income
lm(food_exp ~ income, data = food)
# Displaying statistics for the income column again
favstats(~income, data=food)
#' Interpretation: Intercept, when income(x) is equal to zero,
#' the average food expenditure is $ 83.42
#' income: when income increase by $100 (1 unit), the average 
#' food expenditure increase by $ 10.21
#' income: when income decrease by $100 (1 unit), the average 
#' food expenditure decrease by $ 10.21

#' assign regression model to an object called fit
#' Storing the linear regression model in a variable called fit
fit <- lm(food_exp~income, data=food)
# Displaying the names of the components of the fit object
names(fit)

#' Figure 2.8. The fitted regression
#' Plotting the fitted regression model
plotModel(fit) 

#' Manually, base R
#' Plotting food expenditure against income using base R graphics
plot(food_exp ~ income, data=food, main = "food expenditure against income")
#' Adding the regression line to the plot
abline(fit, col="red", lwd=2)

#' Estimated model (similar, buit not identical to Figure 2.9)
#' Displaying the regression results using the stargazer package
library(stargazer)
stargazer(fit, type="text", intercept.bottom=FALSE)

#' A more general output from the model is given by
#' Displaying a detailed summary of the regression model
summary(fit)
#' Displaying a detailed summary of a new regression model (though it's the same model as before)
summary(lm(food_exp~income, data=food))

#' The OLS parameter estimates
#' Displaying the regression model
fit
#' Extracting and displaying the coefficients of the regression model
coef(fit) # vector
coef(fit)[1] 
coef(fit)[2]

#' using the name of the vector
#' Accessing specific coefficients by name
coef(fit)["income"]
coef(fit)["(Intercept)"]

#' Save the coeficients as Values for later use
#' Storing the coefficients in separate variables
b1 <- coef(fit)[1]
b1
b2 <- coef(fit)[2]
b2

#' Calculate the Income elasticity at mean values, p. 64
#' Calculating the income elasticity at the mean values of income and food expenditure
income.elast.mean <- b2*mean(food$income)/mean(food$food_exp)
income.elast.mean
#' Interpretation: When income increase by 1%, food expenditure increase by 0.7%.  
#' Interpretation: When income increase by 1%, food expenditure increase by `r income.elast.mean`.  
#' Interpretation: When income increase by 1%, food expenditure increase by `r round(income.elast.mean*1,2)`.  

#' Calculating the change in food expenditure for various changes in income
#' When income decrease by 1%, food expenditure decrease by:
round(income.elast.mean*-1, 2)
#' When income increase by 10%, food expenditure increase by:
round(income.elast.mean*10, 2)
#' When income decrease by 5%, food expenditure decrease by:
round(income.elast.mean*-5, 2)

#' st.error of elasticity using the delta method (Chapter 5)
#' Loading the car package for advanced regression diagnostics
library(car)
#' Calculating the mean values of income and food expenditure
mx <- mean(food$income)
my <- mean(food$food_exp)

#' Calculating the standard error of the elasticity using the delta method
deltaMethod(fit, "b2*mx/my", parameterNames=paste("b", 1:2, sep=""))

#' This creates the Income elasticity (at every observation) as a variable
#' Calculating the income elasticity for each observation
income.elast <- b2*food$income/food$food_exp
#' Plotting the income elasticity values
plot(income.elast)
#' Adding a horizontal line at the mean value of the income elasticity
abline(h=mean(income.elast))

#' Note that the Income elasticity calculated at mean x and y values, and the
#' average of the Income elasticity at all observations differ a bit.
#' Displaying the mean of the income elasticity values
mean(income.elast)
#' Displaying the income elasticity calculated at the mean values of income and food expenditure
income.elast.mean

#' Prediction at income=20
#' Creating a function to make predictions based on the regression model
f <- makeFun(fit)
#' Making predictions for various values of income
f(income=20)
f(mean(food$income))
f(mean(food$income), interval="confidence")
f(20, interval="confidence", level=0.9)
f(20)
f(0:40)
f(food$income) # predicted values on the regression line

#' Plotting the predicted values on the regression line
plot(food$income,f(food$income)) # these are the data points on the regression line

#' Elasticity at different points anlong the regression line
#' Creating a function to calculate the elasticity at different points along the regression line
e <- function(income) {b2*income/f(income)}
#' Calculating the elasticity at the mean value of income
e(mean(food$income)) # at mean
#' Plotting the elasticity at different levels of income
curve(e(x), 0,40, xlab = "income", main = "Food expenditure elasticity at different levels of income")

#' Repeated sampling (mosaic::do) or bootstrapping
#' Illustrating 2.4.4. the variance of b1 and b2
#' Resampling the 'food' dataset
resample(food)

#' Bootstrap 100 regressions to estimate the parameters of the linear regression model
table2_2 <- do(100)*lm(food_exp~income, data=resample(food))
table2_2

#' Plotting the original data and adding the regression lines from the bootstrapped samples
plot(food$income, food$food_exp, col="blue", pch=19, main = "Bootstrap parameter variance")
for(i in 1:length(table2_2$.index)) {
  curve(table2_2[i,1]+table2_2[i,2]*x, 0, 40, add = TRUE, col='#FF000088')
}
abline(h=mean(food$food_exp))
abline(v=mean(food$income))

#' Extracting residuals from the model 'fit'
names(fit)
resid(fit)

#' Calculating residuals manually and comparing them with the residuals from the model
cbind(food$food_exp-f(food$income), resid(fit))
all.equal(food$food_exp-f(food$income), resid(fit), tolerance=1/1e9)

#' Checking the lengths of manually calculated residuals and model residuals
length(food$food_exp-f(food$income))
length(resid(fit))

#' Calculating the sum of squared residuals from the model and comparing it with manual calculation
deviance(fit)
sum(resid(fit)^2)

#' Generating the ANOVA table for the model
anova(fit)

#' Calculating the sum of squares due to regression and total sum of squares
sum((f(food$income)-mean(food$food_exp))^2)
sum((f(food$income)-mean(food$food_exp))^2)+sum(resid(fit)^2)
sum((food$food_exp-mean(food$food_exp))^2)

#' Calculating the variance in the model
deviance(fit)/fit$df.residual

#' Extracting the variance-covariance matrix of the model parameters
vcov(fit)

#' Calculating the residual standard error
summary(fit)
sqrt(deviance(fit)/fit$df.residual) # df is N-2

#' -------------------------------
#' Other economic models, 2.3.2
#' Estimating an "iron" model where the response is the iron ore price and the predictor is the exchange rate
rm(list=ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/iron.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/iron.rdata"))

ironmodel <- lm(iron ~ xrate, data = iron)
summary(ironmodel)

#' Calculating the elasticity of the model
coef(ironmodel)[2]*mean(iron$xrate)/mean(iron$iron)

#' -------------------------------
#' Example 2.6
rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/br.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))

head(br)
#' Estimating a regression model for house prices using square footage as a predictor
fit <- lm(price~I(sqft^2), data=br)
summary(fit)

#' Plotting the data and fitting a regression model
with(br, plot(sqft,price))
br <- br %>% mutate(sqft2 = sqft^2)
lm(price~sqft2, data=br)

#' Extracting model details using the 'broom' package
library(broom)
tidy(fit)
glance(fit)
augment(fit)

#' Extracting model estimates and plotting the model
fit_stats <- tidy(fit)
fit_stats$estimate
fit_stats$estimate[2] # slope

plotModel(fit)
f <- makeFun(fit)
f(2000)
f(6000)

#' Calculating the slope of the model at different values of square footage
slope <- function(sqft) {2*coef(fit)[2]*sqft}
slope(c(2000,4000,6000))

#' Plotting the data with price on one axis and the slope on another axis
library(latticeExtra)
obj1 <- xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
obj2 <- plotFun(slope(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

#' Calculating the elasticity of the model
elasticity <- function(sqft) {slope(sqft)*sqft/f(sqft)}
obj3 <- plotFun(elasticity(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj3, add.ylab2 = TRUE)
stats <- favstats(~sqft, data = br)
ladd(panel.abline(v=c(stats$Q1,stats$Q3),  lwd=3, alpha=.4 ) ) # vertical line at Q1 and Q4
elasticity(c(stats$Q1,stats$median,stats$Q3))

elasticity(c(2000,4000,6000))
f(c(2000,4000,6000))

#' -------------------------------
#' Example 2.7 a log-linear function
#' Estimating a log-linear regression model for house prices using square footage as a predictor
rm(list = ls())

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))
head(br)

#' Plotting histograms of house prices and log-transformed house prices
histogram(~price, width=10000, data=br)
histogram(~log(price), width=0.1, data=br)

#' Estimating the regression model and plotting the data
fit <- lm(log(price)~sqft, data=br)
summary(fit)

with(br, plot(sqft,price)) # original data in levels
with(br, plot(sqft,log(price))) # log transformed price and sqft in levels

br <- br %>% mutate(logprice = log(price))
lm(logprice~sqft, data=br)

#' Extracting model details using the 'broom' package
tidy(fit)
glance(fit)
augment(fit)

#' Plotting the model and the data
xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
f <- makeFun(fit)
plotFun(f(sqft) ~ sqft, col="red", xlim = c(0,8000), add=TRUE)
g <- function(sqft) {exp(coef(fit)[1]+coef(fit)[2]*sqft)} # manually using the exponential function exp()
plotFun(g(sqft) ~ sqft, col="green", xlim = c(0,8000), add=TRUE)

#' Plotting the fitted curve of the model
library(rockchalk)
plotCurves(fit, plotx = "sqft")

#' Calculating the slope of the model at different values of house prices
slope <- function(price) {coef(fit)[2]*price}
slope(c(100000,500000))

#' Calculating the slope of the model at different values of square footage
slope2 <- function(sqft) {coef(fit)[2]*f(sqft)}

#' Finding the square footage values corresponding to specific house prices
s1 <- findZeros(f(sqft)-100000~sqft) ; s1
f(s1)
s2 <- findZeros(f(sqft)-500000~sqft) ; s2
f(s2)

slope(c(100000,500000))
slope2(c(s1$sqft,s2$sqft))

#' Plotting the data with price on one axis and the slope on another axis
obj1 <- xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
obj2 <- plotFun(slope2(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

#' Plotting the fitted line and the slope
obj3 <- plotFun(f(sqft) ~ sqft, col="blue", xlim = c(0,8000))
doubleYScale(obj2, obj3, add.ylab2 = TRUE)

#' Calculating the elasticity of the model
elasticity <- function(sqft) {coef(fit)[2]*sqft}
elasticity(c(2000,4000))

#' Plotting the data with price on one axis and the elasticity on another axis
obj4 <- plotFun(elasticity(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj4, add.ylab2 = TRUE)

#' Plotting the fitted price and the elasticity
doubleYScale(obj3, obj4, add.ylab2 = TRUE)

#' Calculating the increase in price for a 100 square foot increase
100*100*coef(fit)[2]
#' percent (elasticity, equation 2.28).
#' Note: first 100 is percent second 100 is unit

#' ---------------------------------------------------------
#' 2.9 Regression with indicator variables
# Estimating a regression model for house prices using various predictors including indicator variables
rm(list = ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))

# price	house price, in $1000	
# sqft		square feet of living area, in 100's	
# age		house age, in years	
# utown	=1 if close to university	
# pool		=1 if house has pool	
# fplace	=1 if house has fireplace

#' Displaying the first few rows of the 'utown' dataset
head(utown)

#' Plotting histograms of house prices for different values of the 'utown' variable
histogram(~price|utown, data = utown)
histogram(~price|as.factor(utown), layout=(c(1,2)), data = utown)

#' Plotting density plots of house prices for different values of the 'utown' variable
utown %>% ggplot(aes(x=price, fill=as.factor(utown))) + geom_density(alpha=0.25)

#' Calculating summary statistics of house prices for different values of the 'utown' variable
favstats(~price|utown, data=utown)
mean(~price|utown, data=utown)

#' Performing a t-test to compare the means of house prices for different values of the 'utown' variable
t.test(price~utown, data=utown, var.equal=TRUE)
ttest <- tidy(t.test(price~utown, data=utown, var.equal=TRUE))
ttest

#' Performing an ANOVA to compare the means of house prices for different values of the 'utown' variable
summary(aov(price~utown, data=utown))
ttest$statistic^2

mean(~price, data=utown) # mean independent of group, i.e, utown 0 or 1, or grand mean	
lm(price~1, data=utown) # mean independent of group, i.e, utown 0 or 1, or grand mean	
#' Mean per group	
mean(~price|utown, data = utown)	
diffmean(~price|utown, data = utown)	
diffmean(price~utown, data = utown)	
means <- mean(~price|utown, data = utown)	
means	
#' Note that sqft is not in this model, I just use it to get a scatterplot!	
xyplot(price ~ sqft, groups = utown, data = utown, auto.key=TRUE)	
ladd(panel.abline(h=c(means[1],means[2]), lwd=3, alpha=.4, col=c("blue","red")))	

#' Estimating a regression model with the 'utown' variable as a predictor
fit <- lm(price~utown, data=utown)
fit
summary(fit)

#' Calculating the mean house price for different values of the 'utown' variable
mean(~price, data=filter(utown, utown==0))
mean(~price, data=filter(utown, utown==1))

#' Reversing the coding of the 'utown' variable and estimating a regression model
utown <- utown %>% mutate(REVutown = ifelse(utown==1,0,1))
with(utown, table(utown,REVutown))
REVfit <- lm(price~REVutown, data=utown)
REVfit

#' Displaying the summary of the original and reversed models
summary(fit)
summary(REVfit)

