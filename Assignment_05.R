
# 4.16

rm(list=ls())

library(mosaic)
library(tidyverse)

#' The data defenition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")

# Load the data:
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))
head(newbroiler)
tail(newbroiler)

#' a).

#' The estimated reciprocal model
#'  Q = a_1 + a_2*(1/p) + e 
fit1 <- lm(q~I(1/p), data=newbroiler)
summary(fit1)

plotModel(fit1)

#or 
newbroiler %>% ggplot(aes(x=p, y=q)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(1/x)) 

#' The reciprocal model fits the data relatively well.
#' There is some tendency to overestimate quantity in 
#' the middle range of prices and
#' underestimate quantity at the low and high extreme prices. 


#' It is also possible to create a new variable that is 
#' the reciprocal of price,and use this in a regression.



#' b) compute the elasticity of per capita consumption w.r.t. real price

#' Finding the derivative:
D(expression(1/p), "p")

D(expression(b1+b2*1/p), "p")

f1 <- makeFun(fit1)

#' Elasticity
e <- function(p) {coef(fit1)[2]*-(1/p^2)*p/f1(p)}

median(~p, data=newbroiler) # median p
f1(median(~p, data=newbroiler)) # q at median p

e(median(~p, data=newbroiler)) # elasticity at median p


curve(e(x), 0.9,3) # Higher prices, more elastic


##########################################
# log-log model 
fit_log_log <- lm(log(q) ~log(p), data = newbroiler)
coef(fit_log_log)
coef(fit_log_log)[2]

#'The elasticity found using the log-log model is -1.121 , 
#'a similar, but slightly smaller absolute value than that
#' for the reciprocal model

e(mean(~p, data=newbroiler)) # elasticity at median p
########################################


#' c.  linear-log model 

fit2 <- lm(q~log(p), data=newbroiler)
summary(fit2)

plotModel(fit2)

#' Like the reciprocal model, this linear-log model tends to 
#' under predict for low and high prices and
#' over predict for mid-range prices. 
#' Also, its fit appears slightly worse than that of
#' the reciprocal model. 



#' d). compute the elasticity 

#' Finding the derivative:
D(expression(log(p)), "p")

D(expression(b1+b2*log(p)), "p")

f2 <- makeFun(fit2)

#' Elasticity
e2 <- function(p) {coef(fit2)[2]*(1/p)*p/f2(p)}

median(~p, data=newbroiler) # median p
f2(median(~p, data=newbroiler)) # q at median p
e2(median(~p, data=newbroiler)) # elasticity at median p


#'compare the elasticities:
#' The linear-log model yields a more inealastic 
#' elasticity (in absolute value) than the other model.

curve(e2(x), 0.9,3, col="red") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model



#' e. Log-linear function 

fit3 <- lm(log(q)~p, data=newbroiler)
summary(fit3)

f3 <- makeFun(fit3)

#plotModel(fit3) doesnot work here 
xyplot(q~p, data=newbroiler) 
plotFun(f3(p) ~ p, add=TRUE, col="darkred", lwd=2)

#' The model fits well at low prices but over predicts 
#' in the middle range, and under predicts at higher prices. 


#' f. compute the elasticity 
e3 <- function(p) {coef(fit3)[2]*p}

median(~p, data=newbroiler) # median p

f3(median(~p, data=newbroiler)) # q at median p

e3(median(~p, data=newbroiler)) # elasticity at median p

#' The elasticity from this model is lower than for the other models.


#' Plot the elasticities from all all 3 models
curve(e2(x), 0.9,3, col="red", xlab = "price", ylab = "elasticity") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model
curve(e3(x), 0.9,3, col="blue", add = TRUE) # Log-lin model
legend("bottomleft", inset=.05, title="Models", #legend: topright
       c("Linear-Log","Reciprocal","Log-Lin"), fill=c("red","black","blue"), horiz=TRUE)

#' The generalized R^2 is:
cor(f3(newbroiler$p),newbroiler$q)^2


# g.  Evalute the suitability of the alternative model


# Plot the residuals from each model 

# Reciprocal model
newbroiler$resid_rec <- resid(fit1) # include the residuals from the model into our data frame
newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
#'The residual scatter shows a definite spray 
#'pattern, suggesting heteroskedastic errors. 

#log-log 
newbroiler$resid_log_log <- resid(fit_log_log)
newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 

#linear_log 
newbroiler$resid_lin_log <- resid(fit2)
newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
#'The residual scatter shows a definite spray 
#' pattern, suggesting heteroskedastic errors. 


#'log-linear 
newbroiler$resid_log_lin <- resid(fit3)
newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 
#' The residual scatter shows a U-shape, suggesting a missed
#'  quadratic or cubic component, or at least a misspecification. 


#All residuals on the same plot 
library(gridExtra)
Recip <-newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
log_log<- newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 
lin_log <- newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
log_lin <-newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 


grid.arrange(Recip,log_log,lin_log,log_lin, ncol=2)


#' Given the above diagnostics, the log-log model seems to be
#'  the best choice, based on a less-well defined residual plot. 


# check Normality of the residuals 
# H0: Normality vs H1: not normal 
library(tseries)
jarque.bera.test(resid(fit1)) #Reciprocal Model
jarque.bera.test(resid(fit_log_log)) #Log-Log Model
jarque.bera.test(resid(fit2)) #Lin-Log Model
jarque.bera.test(resid(fit3)) #Log-lin Model

#conclusion: the residuals from all the models are normally distributed 



#  4.20

#a
#E(y)=exp(b1+b2x) where b1=beta_1_hat and b2=beta_2_hat because E(e)=0

#b
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
head(cps5_small)
#Estimating the model
m4=lm(log(wage)~educ, data=cps5_small[1:1000,])
summary(m4)
sigma_sq=var(residuals(m4)) # the variance of the residuals
c=exp(sigma_sq/2)
c
#the value of c=1.12258

#(c)
d=sum( exp(residuals(m4))/1000)
d
#the value of d is 1.122, which is approximately equal to the 
#correction factor.

#(d)
Last_200=cps5_small[1001:1200,]    #the last 200 observations
yn_hat=exp(coef(m4)[1]+coef(m4)[2]*Last_200$educ)
yc_hat=c*yn_hat
yd_hat=d*yn_hat
Mse_n=sum((yn_hat-Last_200$wage)^2)/200
Mse_n
Mse_c=sum((yc_hat-Last_200$wage)^2)/200
Mse_c
Mse_d=sum((yd_hat-Last_200$wage)^2)/200
Mse_d
#The value of MSE_n=378.397, MSE_c=359.13 and MSE_d=359.192
#The one with the minimum value will be the best predicator, hence
#MSE_c is the best however MSE_d is alo very to MSE_c.





#  4.28

rm(list=ls())

library(mosaic)
library(broom)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/wa_wheat.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"))

names(wa_wheat)

dframe <- wa_wheat %>% select(northampton, time) %>% rename(yield=northampton) 
head(dframe)


mod1_lin <- lm(yield ~ time, data = dframe)
summary(mod1_lin)

# some statistics from the estimated model
dframe %>% do(glance(lm(yield ~ time, data = .)))

plotModel(mod1_lin)
mplot(mod2_lin)

# More plots.... 
hist(resid(mod1_lin))

augment(mod1_lin)
resid.mod1_lin <- augment(mod1_lin)
resid.mod1_lin %>% ggplot(aes(x=time,y=.resid))+geom_point()


# The  lin-log model 
mod2_lin_log <- lm(yield ~ log(time), data = dframe)
summary(mod2_lin_log)

plotModel(mod2_lin_log)
#mplot(mod2_lin_log)
dframe %>% do(glance(lm(yield ~ log(time), data = .)))

mod3_sq <- lm(yield ~ I(time^2), data = dframe)
summary(mod3_sq)

plotModel(mod3_sq)
#mplot(mod3_sq)
dframe %>% do(glance(lm(yield ~ I(time^2), data = .)))

mod4_log_lin <- lm(log(yield) ~ time, data = dframe)
summary(mod4_log_lin)
#plotModel(mod4_log_lin)
f4 <- makeFun(mod4_log_lin)
xyplot(yield ~ time, data = dframe)
plotFun(f4(time) ~ time, add=TRUE, col="red")

#mplot(mod4_log_lin)
dframe %>% do(glance(lm(log(yield) ~ time, data = .)))
library(gridExtra)
mplot(mod4_log_lin, which = 1:7, multiplot = TRUE, ncol = 2)

#' Jarque Bera test H0: Residuals are Normal
jarque.bera.test(resid(mod1_lin))
jarque.bera.test(resid(mod2_lin_log))
jarque.bera.test(resid(mod3_sq))
jarque.bera.test(resid(mod4_log_lin))

gf_histogram(~resid(mod1_lin))
gf_histogram(~resid(mod2_lin_log))
gf_histogram(~resid(mod3_sq))
gf_histogram(~resid(mod4_log_lin))

#' Comparing the quadratic and log-linear fitted curves we see that both capture the shape of the relationship.
#' The quadratic model has a higher R^2 and the residual plot does not show as much of a dip in the center region.
#' Thus, we choose the quadratic model as our preferred specification.

# linear model
dframe %>% do(tidy(lm(yield ~ time, data = .))) %>% filter(term=="time") %>% select(term,estimate)
# linear-log model, dy/dx = beta*1/x
dframe %>% do(tidy(lm(yield ~ log(time), data = .))) %>% filter(term=="log(time)") %>% select(term,estimate)
# squared model, dy/dx = 2*beta*time
dframe %>% do(tidy(lm(yield ~ I(time^2), data = .))) %>% filter(term=="I(time^2)") %>% 
  select(term,estimate) %>% summarise(2*estimate)
# log-lin model, dy/dx = beta*time
dframe %>% do(tidy(lm(log(yield) ~ time, data = .))) %>% filter(term=="time") %>% select(term,estimate)

# linear model
# Studentized residual threshold = 2
dframe %>% do(augment(lm(yield ~ time, data = .))) %>% arrange(-.std.resid)
dframe %>% do(augment(lm(yield ~ time, data = .))) %>% arrange(.std.resid)

mod3 <- lm(yield ~ I(time^2), data = filter(dframe, time <= 47))
f <- makeFun(mod3)
f(48, interval="prediction")
dframe[48,]

