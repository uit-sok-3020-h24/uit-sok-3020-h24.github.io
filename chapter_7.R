

#' ## Chapter 7 - Using Indicator Variables

library(mosaic)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))
#' Table 7.1
head(utown)
noquote(names(utown))

# price	house price, in $1000
# sqft		square feet of living area, in 100's
# age		house age, in years
# utown	=1 if close to university
# pool		=1 if house has pool
# fplace	=1 if house has fireplace

fit <- lm(price~utown+sqft+I(utown*sqft)+age+pool+fplace, data = utown)
#' Table 7.2
summary(fit)

f <- makeFun(fit)

#' The intercept, the expected price, when all explanatory 
#' variables are zero.
#' E(price|utown=0,sqft=0,age=0,pool=0,fplace=0)
f(utown=0,sqft=0,age=0,pool=0,fplace=0)

#' The location premium for lots near the university,
#'  calculated as a difference between expected values
f(utown=1,sqft=0,age=0,pool=0,fplace=0)-f(utown=0,sqft=0,age=0,pool=0,fplace=0)
#' aka. the coefficient on utown
coef(fit)[2]

#' The change in expected price per additional square foot is
(coef(fit)[3]+coef(fit)[4])*10
#' $ for houses near the university and
coef(fit)[3]*10
#' $ for houses in other areas.

#' Houses depreciate by 
coef(fit)[5]*1000
#' $ per year

#' A pool increases the value of a home by $
coef(fit)[6]*1000

#' A fireplace increases the value by $
coef(fit)[7]*1000

#' A house near the university, of 25000 square feet, 
#' being 10 years old,
#' with no pool and fireplace is sold for $
f(utown=1,sqft=25,age=10,pool=0,fplace=0)*1000



#' ### Example 7.2 The Effects of Race and Sex on Wage

rm(list=ls())

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5_small.def")

#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
head(cps5_small)
noquote(names(cps5_small))

cps5_small <- cps5_small %>% mutate(black.female=black*female)

fit <- lm(wage~educ+black+female+black.female, data = cps5_small)
#' Table 7.3
summary(fit)

#' Holding the effect of education (educ) constant, a black male earn
coef(fit)[3]
#' per hour less than a white male, white females earn
coef(fit)[4]
#' per hour less than a white male, and black females earn
coef(fit)[3]+coef(fit)[4]+coef(fit)[5]
#' per hour less than a white male.

#' p. 324: Test the joint significance of all the qualitative factors
#' The model is: wage= b1 Intercept + b2 educ + b3 black + b4 female + b5 black.female

#' 3-joint hypothesis, from chapter 6.7
#' H0: b3 = 0, and
#' H0: b4 = 0, and
#' H0: b5 = 0
library(car)

Hypothesis <- matrix(c(0,0,1,0,0,
                       0,0,0,1,0,
                       0,0,0,0,1), 3, 5, byrow=TRUE)
RHS <- c(0,0,0)

colnames(Hypothesis) <- c("b1", "b2", "b3", "b4", "b5")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2", "eqtn 3")
Hypothesis

linearHypothesis(fit, Hypothesis, rhs=RHS)
#' Thus we conclude that a workers race and/or sex affect the wage equation



#' Example 7.3 A wage equation with regional indicators
fit2 <- lm(wage~educ+black+female+black.female+south+midwest+west, data = cps5_small)
#' Table 7.4
summary(fit2)

#' Test of no regional effects, joint test on the regional dummies.
Hypothesis <- matrix(c(0,0,0,0,0,1,0,0,
                       0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,1), 3, 8, byrow=TRUE)
RHS <- c(0,0,0)

colnames(Hypothesis) <- c("b1","b2","b3","b4","b5","b6","b7","b8")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2", "eqtn 3")
Hypothesis

linearHypothesis(fit2, Hypothesis, rhs=RHS)
#' We keep the H0, no regional effects.



#' Testing the equivalence of two regressions, the chow test
noquote(names(cps5_small))

cps5_small <- cps5_small %>% mutate(educ.south=educ*south,
                                    black.south=black*south,
                                    female.south=female*south,
                                    black.female.south=black*female*south)

fit3 <- lm(wage~educ+black+female+black.female+south+educ.south+black.south+female.south+black.female.south,
           data = cps5_small)

#' Table 7.5, Whole sample
summary(fit3)

#' Table 7.5, Nonsouth
fit4 <- lm(wage~educ+black+female+black.female, 
           data = filter(cps5_small, south==0))
summary(fit4)

#' Table 7.5, South
fit5 <- lm(wage~educ+black+female+black.female, 
           data = filter(cps5_small, south==1))
summary(fit5)

#' Verify that the coefficients from the whole sample is equal to the separate models.

#' In the nonsouth, a black female has
coef(fit4)[5]
#' This is the same coefficient as the whole sample
coef(fit3)[5]

#' In the south, a black female has
coef(fit5)[5]
#' This is the same coefficient as the whole sample
coef(fit3)[5]+coef(fit3)[10]

#' However, their differences can only be tested on the whole sample,
#' using a joint hypothesis

#' Example 7.4 Test the joint hypothesis
#' Note that we are using the names of the variables instead of the matrix approach above!
joint.hyp <- c("south=0",
               "educ.south=0",
               "black.south=0",
               "female.south=0",
               "black.female.south=0")

linearHypothesis(fit3,joint.hyp)

#' We fail to reject the H0, the regression in the south is no different from that of the whole country




rm(fit,fit2,fit3,fit4,fit5,Hypothesis,joint.hyp,RHS)

#' Example 7.5 Indicator Variables in Log-Linear Models

fit <- lm(log(wage)~educ+female, data=cps5_small)

approx <- round(100*coef(fit)[3],3)
exact <- round(100*(exp(coef(fit)[3])-1),3)

paste(approx,exact)

# -----------------------------------

#' Project STAR, an application of the simple difference estimator
#' 

rm(list=ls())

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/star.def")

# star.def
# 
# id schid  tchid  tchexper absent  readscore mathscore  totalscore boy 
# white_asian black  tchwhite tchmasters  freelunch  schurban schrural 
# small regular aide              
# 
# Obs:   5786 observations
# 
# id 		student id
# schid	school id
# tchid	teacher id
# tchexper	teacher years of experience
# absent	days absent
# readscore	reading score
# mathscore	math score
# totalscore	combined math and reading score
# boy		male student
# white_asian	white or asian student
# black	black student
# tchwhite	white teacher
# tchmasters	teacher with masters degree
# freelunch	free lunch provided
# schurban	school urban or inner city
# schrural	school rural
# small	small class
# regular	regular class
# aide		regular class with aide
# 
# Data source: http://www.heros-inc.org/star.htm 
# 
# 
# 
# Variable |       Obs        Mean    Std. Dev.       Min        Max
# -------------+--------------------------------------------------------
#   id |      5786    15593.06    2694.317      10133      21580
# schid |      5786    211001.8    38381.93     112038     264945
# tchid |      5786    2.11e+07     3838193   1.12e+07   2.65e+07
# tchexper |      5766    9.306452    5.767684          0         27
# absent |      5765    10.27511     9.27064          0         79
# -------------+--------------------------------------------------------
#   readscore |      5786    436.7297    31.71347        315        627
# mathscore |      5786     485.599    47.69394        320        626
# totalscore |      5786    922.3287     73.7466        635       1253
# boy |      5786    .5134808    .4998614          0          1
# white_asian |      5786    .6766333    .4678018          0          1
# -------------+--------------------------------------------------------
#   black |      5786    .3209471    .4668809          0          1
# tchwhite |      5786    .8354649    .3707925          0          1
# tchmasters |      5786     .351711    .4775456          0          1
# freelunch |      5786    .4816799    .4997074          0          1
# schurban |      5786    .3128241    .4636834          0          1
# -------------+--------------------------------------------------------
#   schrural |      5786    .4709644    .4991994          0          1
# small |      5786    .3003802    .4584629          0          1
# regular |      5786    .3465261    .4759043          0          1
# aide |      5786    .3530937    .4779728          0          1


#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))

head(star)
noquote(names(star))

# Obs:   5786 observations
# 
# id 		student id
# schid	school id
# tchid	teacher id
# tchexper	teacher years of experience
# absent	days absent
# readscore	reading score
# mathscore	math score
# totalscore	combined math and reading score
# boy		male student
# white_asian	white or asian student
# black	black student
# tchwhite	white teacher
# tchmasters	teacher with masters degree
# freelunch	free lunch provided
# schurban	school urban or inner city
# schrural	school rural
# small	small class
# regular	regular class
# aide		regular class with aide

library(tidyverse)
library(stargazer)

star %>% filter(regular==1) %>%
  select(totalscore,small,tchexper,boy,freelunch,white_asian,tchwhite,tchmasters,schurban,schrural) %>%
  stargazer(. , type="text", header=FALSE,
            title="Table 7.6a Dataset 'star' Summary Statistics for Regular Sized Classes",
            summary.stat = c("n","mean", "sd", "min", "max"))

star %>% filter(small==1) %>%
  select(totalscore,small,tchexper,boy,freelunch,white_asian,tchwhite,tchmasters,schurban,schrural) %>%
  stargazer(. , type="text", header=FALSE,
            title="Table 7.6b Dataset 'star' Summary Statistics for Small Sized Classes",
            summary.stat = c("n","mean", "sd", "min", "max"))

#' Example 7.9 The Difference Estimator with Additional Controls
#' Table 7.7
star.data <- star %>% filter(regular==1 | small==1)

fit <- lm(totalscore~small, data = star.data)
summary(fit)          

fit2 <- lm(totalscore~small+tchexper, data = star.data)
summary(fit2)          

#' School Effects is a dummy variable on schid, computed bu making schid a factor
fit3 <- lm(totalscore~small+as.factor(schid), data = star.data)
summary(fit3)             

#' fixed effects    
fit4 <- lm(totalscore~small+tchexper+as.factor(schid), data = star.data)
summary(fit4)             

stargazer(fit, fit2, fit3, fit4, type = "text", omit = "schid", intercept.bottom = FALSE,
          title="Table 7.7 Project STAR: Kindergarten")

library(broom)
#' Confidence interval for variable small in model 4
confint_tidy(fit4, conf.level = 0.95)[2,]

# ------------------------------------------------------



rm(list=ls())

#'  ## Example 7.12 Estimating the effect of a minimum wage change, the DiD estimator, p. 340
browseURL("https://www.scimagojr.com/journalrank.php?area=2000")

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/njmin3.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/njmin3.rdata"))

head(njmin3)
noquote(names(njmin3))

# Obs:   820
# 
# nj           = 1 if new jersey
# d            = 1 if after nj min wage increase
# d_nj         nj*d interaction
# fte          full time-equivalent employees
# bk           = 1 if burger king
# kfc          = 1 if kentucky fried chicken
# roys         = 1 if roy rodgers
# wendys       = 1 if wendys
# co_owned     = 1 if company owned
# centralj     = 1 if in central nj
# southj       = 1 if in southern nj
# pa1          = 1 if in pa, northeast suburbs of phila
# pa2          = 1 if in pa, easton etc
# demp         change in full time employment

#' Difference in difference estimator
#browseURL("https://www.wolframalpha.com/input/?i=%28%28b1%2Bb2%2Bb3%2Bd%29-%28b1%2Bb3%29%29-%28%28b1%2Bb2%29-b1%29")

#' Table 7.8
#' Pennsylvania
favstats(fte~d, data = filter(njmin3, nj==0))
#' New Jersey (NJ)
favstats(fte~d, data = filter(njmin3, nj==1))


fit <- lm(fte~nj+d+d*nj, data=njmin3)
fit2 <- lm(fte~nj+d+d*nj+kfc+roys+wendys+co_owned, data=njmin3)
fit3 <- lm(fte~nj+d+d*nj+kfc+roys+wendys+co_owned+southj+centralj+pa1, data=njmin3)

stargazer(fit,fit2,fit3, type="text",
          title="Table 7.9 Difference in Differences Regressions",
          header=FALSE, keep.stat="n",digits=2, single.row=TRUE, intercept.bottom=FALSE)

#' The coefficient on the term nj:d in Table 7.9 is delta, our difference-in-differences estimator. 
#' The t-statistic for delta in model 1 is:
tidy(fit)
summary(fit)

library(multcomp)
summary(glht(fit, linfct = c("nj:d >= 0"))) # multcomp::glht, one sided test, H0: delta >=0
#' Keep H0, no significant reduction in employment

#' The figure below displays the change of the effect of minimum wage from the period before (d=0) to the period after
#' the change in minimum wage (d=1) for both the treatment and the control groups.
#' The line labeled "counterfactual" shows how the treatment group would have changed in the absence of the treatment,
#' assuming its change would mirror the change in the control group. The graph is plotted using the first model (fit).
fit
b1 <- coef(fit)[1]
b2 <- coef(fit)[2]
b3 <- coef(fit)[3]
delta <- coef(fit)[4]
#' Using the notation from Figure 7.3, p. 339
C <- b1+b2+b3+delta
E <- b1+b3
B <- b1+b2
A <- b1
D <- E+(B-A)
#' Difference-in-Differences Plot
#' Pennsylvania is the control, and NJ is the treated
plot(1, type="n", xlab="period", ylab="fte", xaxt="n", xlim=c(-0.01, 1.01), ylim=c(18, 24),
     main="Difference-in-Differences")
segments(x0=0, y0=A, x1=1, y1=E, lty=1, col=2, lwd=2) #control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3, lwd=2) #treated
segments(x0=0, y0=B, x1=1, y1=D, lty=4, col=4, lwd=2) #counterfactual
legend("topright", legend=c("control", "treated", "counterfactual"), lty=c(1,3,4), col=c(2,3,4))
axis(side=1, at=c(0,1), labels=NULL)
text(0, 23.5, "A") ; text(0, 20.6, "B") ; text(1, 20.8, "C") ; text(1, 18.5, "D") ; text(1, 21.5, "E")

#' Example 7.13 Using Panel Data, using the differenced data
fit4 <- lm(demp~nj, data=njmin3)
summary(fit4)
#' The value of the estimated difference-in-differences coefficient is very close to the one we estimated before.
#' Its  t-statistic is still positive, indicating that the null hypothesis
#' H0: "an increase in minimum wage increases employment" cannot be rejected.
