

# Final exam, 2023 
rm(list = ls())

library(tidyverse)
heinz <- read_csv("https://raw.githubusercontent.com/oysteinm/data/main/heinz.csv")

# week - Week number (1 to 124).
# sales - Weekly sales in US$ of Heinz ketchup.
# price - Average weekly price in US$ of Heinz ketchup.
# displayonly - A week in which the brand was on display promotion only, a dummy/indicator variable, 1 if so, 0
# otherwise.
# coupononly - A week in which the brand had a coupon promotion only, a dummy/indicator variable, 1 if so, 0 otherwise.
# displaycoupon - A week in which the brand was both on display and had a coupon promotion, a dummy/indicator
# variable, 1 if so, 0 otherwise.


# heinz %>% 
#   mutate(
#     quantity = sales/price,
#     price = price*100,
#     dprice = price-dplyr::lag(price),
#     dlprice = log(price)-dplyr::lag(log(price))
#   ) %>% na.omit() %>% View()


# Item 3
df <- heinz %>% 
  mutate(
    quantity = sales/price,
    price = price*100,
    dprice = price-dplyr::lag(price),
    dlnprice = log(price)-dplyr::lag(log(price))
  ) %>% na.omit()

#Item 4
model1 <- lm(sales ~ dprice+displayonly+coupononly+displaycoupon, data = df)
summary(model1)

#Item 5
model2 <- lm(log(sales) ~ dlnprice+displayonly+coupononly+displaycoupon, data = df)
summary(model2)

#Item 6 

round(coef(model1)[2],2) # the coeff of dprice 
round(coef(model1)[3],2) # the coeff of displayonly 
round(coef(model1)[4],2) # the coeff of cupononly 
round(coef(model1)[5],2) # the coeff of displaycoupon  

#----Interpretation  of coefficient on dprice.......

# look at the p-value in the summary of the regression function or 
library(car)
linearHypothesis(model1,c("displayonly=0"))



#Item 7
# look at the R2 value 
summary(model1)$r.square

#round it to 2 sig. dig
round(summary(model1)$r.square,2)

#Item 08
library(mosaic)
f= makeFun(model1)
f(-5,0,0,1,interval="confidence",level=0.95)

round(f(-5,0,0,1,interval="confidence",level=0.95),2)

# the standard error to compute the interval above  

car::deltaMethod(model1,"b1-5*b2+b5", parameterNames = paste("b",1:5,sep = ""))



#Item 9 elasticity 
mean.dprice <- mean(df$dprice) # price is used in the solution paper
mean.sales <- mean(df$sales)

coef(model1)[2]*mean.dprice/mean.sales

# the standard deviation and estimates 
library(car)
car::deltaMethod(model1,"b2*mean.dprice/mean.sales", parameterNames = paste("b",1:5, sep = ""))



# the own-price elasticity 
mq <- mean(df$quantity)
car::deltaMethod(model1, "(b2/mq)-1", parameterNames = paste("b",1:5, sep = ""))


# Item 10 

# In model 2 above, i.e., 
model2 <- lm(log(sales) ~ dlnprice+displayonly+coupononly+displaycoupon, data = df)
summary(model2)

# interpretation of coefficient of displayonly ---- this is dummy variables and 
# the dependent variables is in log form ....do you remember the formual:
# exp(b3)-1

exp(coef(model2)[3])-1

100*( exp(coef(model2)[3])-1)

# the standard error 
car::deltaMethod(model2, "exp(b3)-1", parameterNames = paste("b",1:5, sep = ""))

# Item 11

#  R^2

s2 <- deviance(model2)/model2$df.residual 

exp(s2/2) # the correction afctor 

pred.sales <- exp(predict(model2))*exp(s2/2) # notice the correction factor here 

# Rg^2 = cor(sales, pred.sales)^2
cor(df$sales,pred.sales)^2

# if you round it to 2 sig.dig
round(cor(df$sales,pred.sales)^2, 2)


# Item 12

plot(heinz$week,heinz$price, type = "l")

# Item 13
f=makeFun(model2)
f(-0.05, 0, 0, 1, interval = "confidence")*exp(s2/2) # notice the corection here 


# Item 14

coef(model2)[2]*mean(df$sales)/mean(df$price)

round(coef(model2)[2]*mean(df$sales)/mean(df$price),2)

ms <- mean(df$sales)
mp=mean(df$price)

car::deltaMethod(model2, "b2*ms/mp",parameterNames =paste("b",1:5, sep = ""))



# Hypothesis test 

# profit.displayonly -40
#profit.coupononly -150
#profit.combined -190


car::deltaMethod(model1,"b3-40",parameterNames =paste("b",1:5, sep = ""))
car::deltaMethod(model1,"b4-150",parameterNames =paste("b",1:5, sep = ""))
car::deltaMethod(model1,"b5-190",parameterNames =paste("b",1:5, sep = ""))

# using the Linear Hypothesis function 
car::linearHypothesis(model1,"coupononly-40=0")
car::linearHypothesis(model1,"displayonly-150")
car::linearHypothesis(model1,"displaycoupon-190")

# combined test 
car::deltaMethod(model1, "b3+b4+b5-380", parameterNames=paste("b",1:5, sep = ""))

car::linearHypothesis(model1, c("coupononly+displayonly+displaycoupon=380"))

library(multcomp)
summary(glht(model1, linfct = c("coupononly+displayonly+displaycoupon=380")))







# Final exam, 2022
rm(list = ls())
library(mosaic)
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))
names(tuna)

fit <- lm(sal1 ~ apr1+apr2+apr3+dispad, data = tuna)
summary(fit)

confint(fit, level = 0.90)[2,]

car::linearHypothesis(fit, c("apr1=0","apr2=0","apr3=0","dispad=0"),level=0.95)

qf(0.95,4,47,lower.tail = TRUE)

# own price elasticies 
maprl<- 0.8
mapr2 <- mean(tuna$apr2)
mapr3 <- mean(tuna$apr3)

# with the display 

f=makeFun(fit)

f(0.8,mapr2,mapr3,1) # predicted sales with dispad 
f(0.8,mapr2,mapr3,0) # predicted sales without dispad 

# elasticity when dispad = 1
pswdis <- f(0.8,mapr2,mapr3,1) # predicated sales with dispad=1
car::deltaMethod(fit, "b2*0.8/pswdis", parameterNames=paste("b",1:5,sep = ""))

# elasticity when dispad= 0
pswtdis <- f(0.8,mapr2,mapr3,0)
car::deltaMethod(fit, "b2*maprl/pswtdis", parameterNames=paste("b",1:5,sep = ""))


# item 8
# sale forecast
f(0.8,mapr2,mapr3, 1) # predicted sales before competing firms change thier prices 

f(0.8,mapr2-0.1,mapr3-0.1, 1)## predicted sales after competing firms change thier prices 

# The change in predicted sales as a result that competing firms changes thier prices
f(0.8,mapr2,mapr3, 1)-f(0.8,mapr2-0.1,mapr3-0.1, 1)

# Interms of percentage
(f(0.8,mapr2,mapr3, 1)-f(0.8,x2,x3, 1))/f(0.8,mapr2,mapr3, 1)

car::deltaMethod(fit,"-0.1*b3-0.1*b4",  parameterNames=paste("b",1:5,sep = ""))



