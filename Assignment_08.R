# 8.20 

rm(list=ls())
library(mosaic)


browseURL("http://www.principlesofeconometrics.com/poe5/data/def/br2.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br2.rdata"))

names(br2)
head(br2)

# a). 
fit.ols <- lm(log(price)~log(sqft)+age+I(age^2)+waterfront+traditional,data = br2)
summary(fit.ols)

# The coefficient of LSQFT is an elasticity, and we estimate that a 1% increase 
# in house size is associated with a 0.93% increase
# in house price.

# How can we interprate the coefficient of waterfront?
# Do you remember how to interpret coeff of indicator variable in log-linear model?

# There are two ways: the rough approximation + exact calculation?

# The rough approximation: 
100*coef(fit.ols)[5]

# round to 1 digit
round(100*coef(fit.ols)[5],1)
#Being on a waterfront is estimated to increase house price by
# about 20.4% and a traditional style home sells for about 9.4% less.

# An exact calculation for Water front:
100*(exp(coef(fit.ols)[5])-1)

# round to two digits 
round(100*(exp(coef(fit.ols)[5])-1),1)


# 95% CI for the age at which age begins to have a positive effect on price 
car::deltaMethod(fit.ols,"-b3/(2*b4)", parameterNames=paste("b",1:6, sep = ""),level=0.95)


# b.

# extract  the residual of the ols model and put the residual in the dataframe 
br2$res.ols <- resid(fit.ols)
names(br2)

# NR2 test for hetroskedasticity (inclusing both age and age^2)
fit.ax1 <- lm(I(res^2) ~ age+I(age^2)+waterfront+traditional, data = br2) 
summary(fit.ax1)

NR2 <- dim(br2)[1]*summary(fit.ax1)$r.squared # sample size (N)* R^2 value 
NR2
# critical value 
qchisq(p=0.95,df=4, lower.tail=TRUE) # df = S-1 = 5-1 (estimated parameters except the intercept)

#compare this with the critical value 
NR2 > qchisq(p=0.95,df=4, lower.tail=TRUE) 
#conclusion: Reject the hypothesis of homoskedasticity 

# Droping the variable age, but keeping age^2
fit.ax2 <- lm(I(res^2) ~ I(age^2)+waterfront+traditional, data = br2) 
summary(fit.ax2)

dim(br2)[1]*summary(fit.ax2)$r.squared > qchisq(p=0.95,df=3, lower.tail=TRUE) 
#conclusion: Again we reject the hypothesis of homoskedasticity 


#plot the residual vs age 
dev.off()

br2 %>% 
  ggplot(aes(x=age, y=res.ols))+
  geom_point()
# Yes, the plot shows an increase in variation of the residuals in the middle.



# c). 

library(sandwich)
library(lmtest)

summary(fit.ols)
coeftest(fit.ols, vcov=vcovHC, type = c("HC1") )
#The robust standard errors are slightly larger for each coefficient.
# Hence, which yields a wider interval estimate [35.03, 75.66] for the -b3/(2*b4)
car::deltaMethod(fit.ols,"-b3/(2*b4)", vcov = vcovHC(fit.ols, type = c("HC1")), parameterNames=paste("b",1:6, sep = ""),level=0.95)




# d. h(age^2,waterfront,traditional)

# Calculating the correction h()
fit.ax3 <- lm(log(I(res.ols^2))~ I(age^2)+waterfront+traditional, data = br2)
summary(fit.ax3)

sigma2 <- exp(coef(fit.ax3)[2]*br2$age^2+coef(fit.ax3)[3]*br2$waterfront+coef(fit.ax3)[4]*br2$traditional)

fit.fgls <- lm(log(price)~log(sqft)+age+I(age^2)+waterfront+traditional, weights=I(1/sigma2),data = br2)
summary(fit.fgls)
summary(fit.ols)
#The elasticity of house price with respect
#to house size is a bit smaller, and the coefficient of AGE2 is smaller and insignificant.

car::deltaMethod(fit.fgls,"-b3/(2*b4)", parameterNames=paste("b",1:6, sep = ""),level=0.95)
#' This is a much higher age turning point estimate and much wider interval estimate,
#' suggesting that we do not have very
# precise information about a potential turning point.

#car::deltaMethod(fit.fgls,"-b3/(2*b4)", vcov = vcovHC(fit.fgls, type = c("HC1")), parameterNames=paste("b",1:6, sep = ""),level=0.95)


# e ?

# Extract and include the residual from the transformed model into the data frame 
br2$res.fgls <- resid(fit.fgls)

fit.fgls.ax <- lm(log(I(res.fgls^2)) ~ I(age^2)+waterfront+traditional, data = br2) 
summary(fit.fgls.ax )

dim(br2)[1]*summary(fit.fgls.ax)$r.squared > qchisq(p=0.95,df=3, lower.tail=TRUE) 
#conclusion: Again we reject the hypothesis of homoskedasticity 


# f. 
coeftest(fit.fgls, vcov. = vcovHC, type= c("HC1"))

car::deltaMethod(fit.fgls,"-b3/(2*b4)",vcov = vcovHC(fit.fgls, type = c("HC1")), parameterNames=paste("b",1:6, sep = ""),level=0.95)

# g.

# Our conclusion is that our estimate of the turning point, 
# if there is one, is so imprecise as to
#. provide no information.