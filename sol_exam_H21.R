

# The solution for Exam 2021

rm(list = ls())
library(mosaic)
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))
names(rice5)
head(rice5)

fit <- lm(log(prod) ~ log(area)+log(labor)+log(fert), data = rice5)
summary(fit)

#Item 3
# The coef of log(area) is elasticity.
# For 1% increase, the elasticity is:
coef(fit)[2]
#For 5%, the elasticity will be:
5*coef(fit)[2]

# Item 4
confint(fit, level = 0.9)[3,] # this is for 1% increase , the CI
10*confint(fit, level = 0.9)[3,]  # this is for 10%, the CI

#Item 5 R^2
summary(fit)$r.squared

# Item 6
s2 <- deviance(fit)/fit$df.residual  # the variance in the model, sigma squared

exp(s2/2)  # the correction factor 

#Rg2 
pred2 <- exp(predict(fit))*exp(s2/2) #Rg^2 

cor(rice5$prod,pred2)^2


# Item 7
f<- makeFun(fit)

f(2,150,200, interval="prediction", level=0.9)*exp(s2/2)  # confidence/prediction ???

# Item 8 ---- here you are asked to compute the standard error of the slope 
mean.prd <- mean(rice5$prod)
mean.area <- mean(rice5$area)
car::deltaMethod(fit, "b2*mean.prd/mean.area", parameterNames=paste("b",1:4, sep = ""))



# Item 9

library(multcomp)

#summary(glht(fit, linfct = c("log(area) >=0.5"))) # notice this donesnot run 

rice5 <- rice5 %>%
  mutate(larea=log(area),
         llabor = log(labor),
         lfert = log(fert))
fit <- lm(log(prod) ~ larea+ llabor+lfert, data = rice5)
summary(fit)

summary(glht(fit, linfct = c("larea >=0.5")))

# t-critical value 
qt(0.95,dim(rice5)[1]-4)

#Item 10
summary(glht(fit, linfct = c("larea+llabor+lfert=1")))
# or using the car package 
car::linearHypothesis(fit, c("larea+llabor+lfert=1"))

