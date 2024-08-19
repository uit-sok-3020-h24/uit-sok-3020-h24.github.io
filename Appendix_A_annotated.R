#' Mathematical tools in R
#' ========================

#' Load tidyverse package which includes dplyr, ggplot2 and others
library(tidyverse)

#' Remove all objects in the current workspace
rm(list=ls())

#' Arithmetic operations in R
2+1  
#' Add 2 and 1
4-2  
#' Subtract 2 from 4
2*3  
#' Multiply 2 by 3
1/5  
#' Divide 1 by 5
2^3  
#' 2 raised to the power of 3
8^(1/3)
#' Cube root of 8
8^1/3  
#' This might not be what you want; it's 8 to the power of 1 divided by 3

#' Checking equality
4-2==2  
#' Check if 2 equals 2
1==.9999999999999  
#' Check if 1 is approximately equal to 1 with 13 decimal places 

#' Assignment of values to variables
a <- 2  
#' Assign value 2 to variable a
b <- 5  
#' Assign value 5 to variable b
a       
#' Print value of a
b       
#' Print value of b
c <- 3 ; c  
#' Assign value 3 to variable c and then print its value

#' Basic arithmetic operations using variables
a + b  
#' Add values of a and b
a / b  
#' Divide value of a by value of b

#' Comparisons
a > b  
#' Check if a is greater than b
a < b  
#' Check if a is less than b

c <- 5  
#' Reassign value 5 to variable c
a*c < b*c  
#' Check if product of a and c is less than product of b and c

c <- -4  
#' Reassign value -4 to variable c
a*c < b*c  
#' Check if product of a and c is less than product of b and c

#' Rounding numbers
round(1/3,4)       
#' Round 1/3 to 4 decimal places
round(2*4.7536,2)  
#' Round product of 2 and 4.7536 to 2 decimal places

#' Various mathematical operations
2^0  
#' 2 raised to the power of 0
223^0  
#' 223 raised to the power of 0

2/0  
#' Dividing by zero gives positive infinity
-10/0  
#' Dividing a negative number by zero gives negative infinity

0.8^Inf  
#' 0.8 raised to the power of infinity, x^Inf when 0 < x < 1 is equal to zero
2^Inf  
#' 2 raised to the power of infinity

#' Handling scientific notation
a <- 510000 
b <- 0.00000034 

a*b  
#' Multiply a and b
a/b  
#' Divide a by b

options(scipen=15)   
#' Force R not to use exponential notation 
a / b
b / a
options(scipen=F)    
#' Allow R to use exponential notation 
b / a

#' Work with logarithms
options(scipen=999)  
#' Force R not to use exponential notation 

x <- map_dbl(1:7, ~10^(.x - 1))  
#' Using map_dbl from purrr to iterate over 1:7 and perform calculation
table <- tibble(x=x, log_x=log(x))  
#' Create a data frame with x and its log

table  #' Display the table

log(1000*10000)  
#' Logarithm of product of 1000 and 10000
log(1000)+log(10000)  
#' Sum of logarithm of 1000 and logarithm of 10000

log(1000*10000)==log(1000)+log(10000)  
#' Check if they are equal

a <- log(10*100)
a
exp(a)  
#' Exponential function to reverse the logarithm
exp(1)  
#' e to the power of 1
log(exp(1))  
#' Logarithm of exponential function

curve(log(x), from=0, to=100, main="Natural logarithm")
#' Plot of natural logarithm
curve(log(x), from=0, to=1000000, main="Natural logarithm")  
#' Same plot but with larger range

#' Function definition and usage
f <- function(x) { log(x) }  
#' Define a function f to compute logarithm
f  
#' Display function definition
f(100)  
#' Apply function to value 100
log(100)  
#' Logarithm of 100

#' Work with decimals and percentages
y <- c(3,3.02)  
#' Create a vector y
y

diff(y)  
#' Difference between elements of y
diff(y)/y[1]  
#' Relative difference
100*diff(y)/y[1]  
#' Percentage difference

lny <- log(y)  
#' Logarithm of y
100*diff(lny)  
#' Percentage difference of logarithm values

y <- seq(1,1.25,0.05)  
#' Create a sequence from 1 to 1.25 with increments of 0.05
y <- c(1,y[1]+0.01,y[-1])  
#' Add an element to the vector y
y
percent <- 100*(y-y[1])/y[1]  
#' Compute percentage difference from the first element
logdiff <- 100*(log(y)-log(y[1]))  
#' Compute percentage difference of logarithms from the first element
approx.error <- 100*((percent-logdiff)/logdiff)  
#' Compute approximate error between percent and logdiff

table <- cbind(y, percent, logdiff, approx.error)  
#' Bind columns to form a table
table
table[-1,]  
#' Remove the first row

#' Understanding linear relationships
#' Clear the workspace
rm(list=ls())

#' Define a simple linear function f(x) = 1 + x
f <- function(x) { 1+1*x }

#' Evaluate the function at x=0 to get the intercept
#' Intercept
f(0)

# Define x values
x1 <- 2
x2 <- 4

# Calculate the slope (rise over run) between the two points
m <- (f(x2) - f(x1)) / (x2 - x1)
rise <- f(x2) - f(x1)
run <- x2 - x1

print(paste("Slope:", m))
print(paste("Rise:", rise))
print(paste("Run:", run))

# Create the ggplot
ggplot(data.frame(x = c(-3, 10)), aes(x)) +
  stat_function(fun = f, geom = "line") +
  geom_vline(aes(xintercept = 0), color = "red") +
  geom_hline(aes(yintercept = 0), color = "red") +
  geom_point(aes(x = x1, y = f(x1)), color = "blue", size = 3) +
  geom_point(aes(x = x2, y = f(x2)), color = "green", size = 3) +
  labs(title = "A linear relationship", x = "x", y = "y") +
  coord_fixed(ratio = 1) + # This will ensure a square aspect ratio
  theme_minimal()

#' Derive the function to obtain its slope (derivative)
#' Slope as the derivative
D(expression(1+1*x), "x")

#' Plot the elasticity (a measure of responsiveness) of the function
#' Add vertical line at x=1 and a horizontal line at the calculated elasticity at x=1
#' Define a simple linear function f(x) = 1 + 2*x
f <- function(x) { 1+2*x }
elasticity <- function(x) { 2*x/f(x) }
elasticity(1)

# Create a plot
ggplot(data.frame(x = c(0.1, 10)), aes(x)) +
  stat_function(fun = elasticity, geom = "line") +
  geom_vline(aes(xintercept = 1), color = "red") +
  geom_hline(aes(yintercept = elasticity(1)), color = "blue") +
  labs(title = "Elasticity of y = 1 + 2x at x=1" , x = "x", y = "Elasticity") +
  theme_minimal()

#' Study of derivatives for nonlinear relationships
#'  Nonlinear relationships
rm(list=ls())

#' Differentiation rules for constants and power functions
#' Derivative rule 1: Derivative of a constant
D(expression(c), "x")

#'  Derivative rule 2: Power rule
D(expression(x^n), "x")

#'  Derivative rule 4: Derivative of constant times a power function
D(expression(c*x^n), "x")

#'  Derivative rule 7: Derivatives involving exponential functions
D(expression(exp(x)), "x")
D(expression(exp(a*x+b)), "x")

#'  Derivative rule 8: Derivatives involving logarithms
D(expression(log(x)), "x")
D(expression(log(a*x+b)), "x")

#' Differentiate the function 4x + 1
#'  Example A.1
D(expression(4*x+1), "x")

#' Plot and differentiate the quadratic function x^2 - 8x + 16
#'  Example A.2
curve(x^2-8*x+16, from=-1, to=10, main="quadratic function: x^2 - 8x + 16")
D(expression(x^2-8*x+16), "x")

#' Differentiate the log function and plot its slope (derivative)
#'  The slope (derivative) of the y=log(x) function
D(expression(log(x)), "x")

#' Define and plot the derivative of the log function
#' The slope dy/dx as a function
d <- function(x) { 1/x }
x <- 1:50
d(x)
plot(x, d(x), type="l", main="the derivative of the log function")

#' Define and plot the elasticity of the log function
#' Elasticity = slope * x/y  of the log(x) function
e <- function(x) { (1/x)*(x/log(x)) }
#e <- function(x) { (1/log(x)) }
plot(x, e(x), type="l", main="the elasticity of the log function")

#' Calculate elasticity at specific x-values
e(2)
e(10)

#' For the quadratic function x^2 - 8x + 16, calculate and plot its elasticity
#'  Example A.2
rm(list=ls())
curve(x^2-8*x+16, from=-1, to=10, main="the quadratic function x^2 - 8x + 16")
D(expression(x^2-8*x+16), "x")

f <- function(x) { x^2-8*x+16 }
dy <- function(x) { (2*x-8) }
e <- function(x) { (2*x-8)*(x/f(x)) }

x <- seq(0,4,2)
f(x)
dy(x)
e(x)

#' Combine the results in a matrix and plot elasticity
cbind(x,f(x),dy(x),e(x))
curve(e(x), 0,3, main="the elasticity of the quadratic function x^2 - 8x + 16")

#' Find the partial derivative of a multi-variable function with respect to x
#'  Partial derivative
D(expression(a*x^2+b*x+c*z+d), "x")

#' #' Plot the function 2x and compute the area under the curve for specific intervals
#' #'  Area under the curve
#' curve(2*x,0,1)
#' integrand <- function(x) { 2*x }
#' integrate(integrand, lower = 0, upper = 1)
#' integrate(integrand, lower = 0.2, upper = 0.6)
#' 0.6^2-0.2^2
#' 
#' #' Shade a specific area under the curve on the plot
#' #'  We would like to shade the region represented by
#' #'  P(0.2 < X < 0.6). The first vertex of our polygon is (0.2,0).
#' cord.x <- c(0.2)
#' cord.y <- c(0)
#' #'  The second vertex is (0.2,f(0.2)), where f(0.2) is the value on the y axis of 2*x evaluated at 0.2. 
#' cord.x <- c(cord.x,0.2) 
#' cord.y <- c(cord.y,integrand(0.2))
#' #'  The third and fourth vertices is (0.6,f(0.6)) and (f(0.6),0).
#' cord.x <- c(cord.x,0.6,0.6)
#' cord.y <- c(cord.y,integrand(0.6),0)
#' 
#' curve(2*x,0,1)
#' polygon(cord.x,cord.y,col='aquamarine3')
#' 
#' #' Calculate and display the numerical value of the shaded area on the plot
#' num <- integrate(integrand, lower = 0.2, upper = 0.6) ; num
#' str(num)
#' text(0.4, 0.4, paste(num$value))

###########################################
#' Exercises, page 764 - 767 
###########################################

rm(list=ls())  #' Clear the workspace

#' Load package with dedicated functions.
library(mosaic)

#' Using `mosaic` functions.  

#' Define a function using the makeFun utility from the mosaic package:
f <- makeFun(2+3*x ~ x)  
f(0)  
#' Evaluate the function at x=0
f(2)  
#' Evaluate the function at x=2

#' Plot the function for x in [0,4]:
plotFun(f(x) ~ x, xlim=c(0,4), ylim=c(0,14), main="the linear function 2+3*x")

#' Calculate the derivative of the function with respect to x:
D(expression(2+3*x),"x")

#' Define the elasticity function using makeFun:
e <- makeFun(3*x/f(x) ~ x)

#' Plot the elasticity function for x in [0,4]:
plotFun(e(x) ~ x, xlim=c(0,4), col="brown", main="the elasticity of the linear function 2+3*x")
e(1)  
#' Evaluate the elasticity at x=1

#' Using `base R` functions.  

#' Define the function using base R:
g <- function(x) {2+3*x}
g(0)  
#' Evaluate the function at x=0
g(2)  
#' Evaluate the function at x=2

#' Plot the function for x in [0,4]:
curve(g(x), 0, 4, col="red", main="the linear function 2+3*x")

#' Define the elasticity function using base R:
h <- function(x) {3*x/g(x)}

#' Plot the elasticity function for x in [0,4]:
curve(h(x), 0, 4, col="green", main="the elasticity of the linear function 2+3*x")
h(1)  
#' Evaluate the elasticity at x=1

#' Clear the workspace:
rm(list=ls())

#' A.1

#' Define a function Q in terms of P:
Q1 <- makeFun(-3 + 2*P ~ P)
Q1(0)  
#' Evaluate the function at P=0

#' Plot the function for P in [0,12]:
plotFun(Q1(P)~P, xlim = c(0,12), main="the linear function -3+2*P")

#' Calculate the slope (derivative) of the function with respect to P:
D(expression(-3 + 2*P),"P")

#' Define the slope function:
d1 <- function(P) {2}
d1(2)   
#' Evaluate the slope at P=2
d1(10)  
#' Evaluate the slope at P=10

#' Define and calculate the elasticity as (slope * x / y):
e1 <- function(P) {2 * P/Q1(P)}
e1(10)         
#' Evaluate elasticity at P=10
e1(P=10)       
#' Another way to evaluate elasticity at P=10

#' Plot the elasticity function for P in [2,12]:
plotFun(e1(P) ~ P, xlim = c(2,12), main="the elasticity of the linear function -3+2*P")


#' Define a linear demand function Q as a function of price P
#' 2) Q = 100 - 20*P
Q2 <- makeFun(100 -20*P ~ P)
#' Define the function using makeFun
Q2(0)                        
#' Evaluate the function when P=0

#' Plot the demand curve Q as a function of P for the range [0,12]
plotFun(Q2(P)~P, xlim = c(0,12), main="the linear function 100-20*P")
#' Plotting the function 

#' Calculate the slope (first derivative) of the demand function with respect to P
D(expression(100 -20*P),"P")  
#' Derivative of the function

#' Define a function to represent the constant slope of the demand curve
d2 <- function(P) {-20}
d2(2)   
#' Evaluate slope at P=2
d2(10)  
#' Evaluate slope at P=10

#' Calculate the elasticity of the demand curve as (slope * P / Q)
e2 <- function(P) {-20 * P/Q2(P)}
e2(10)         
#' Evaluate elasticity at P=10
e2(P=10)       
#' Another way to evaluate elasticity at P=10

#' Plot the elasticity as a function of P for the range [0,4]
plotFun(e2(P) ~ P, xlim = c(0,4), main="the elasticity of the linear function 100-20*P")

#' Define a non-linear demand function Q as a function of price P
#' 3) Q=50*P^-2
Q3 <- function(P) {50*(P^(-2))}  
#' Define the function 
Q3(4)                       
#' Evaluate the function when P=4

# Data frame for Q3
P_values_q3 <- seq(0.1, 5, by = 0.1)   # range can be adjusted as needed
df_q3 <- data.frame(P = P_values_q3, Quantity = sapply(P_values_q3, Q3))

#' Plot the demand curve Q3 as a function of P for the range [1,5]
p_q3 <- 
  ggplot(df_q3, aes(x = P, y = Quantity)) + 
  geom_line() +
  ggtitle("Q3 as a function of P") +
  xlab("P") + ylab("Quantity")

p_q3

#' Calculate the slope (first derivative) of the demand function with respect to P
D(expression(50*P^-2),"P")  
#' Derivative of the function

#' Define a function for the slope of this non-linear demand curve
d3 <- function(P) {-(50 * (P^-(2+1) * 2))}
#' Calculate the elasticity of this non-linear demand curve as (slope * P / Q)
e3 <- function(P) {-(50 * (P^-(2+1) * 2)) * P/Q3(P)}

# For the slope function
P_values <- seq(0.1, 3, by = 0.01)
df_d3 <- data.frame(P = P_values, Slope = sapply(P_values, d3))

p_d3 <- 
  ggplot(df_d3, aes(x = P, y = Slope)) + 
  geom_line() +
  ggtitle("Slope as a function of P") +
  xlab("P") + ylab("Slope")

p_d3

# For the elasticity function
P_values_e3 <- seq(0.1, 1, by = 0.01)
df_e3 <- data.frame(P = P_values_e3, Elasticity = sapply(P_values_e3, e3))

p_e3 <- 
  ggplot(df_e3, aes(x = P, y = Elasticity)) + 
  geom_line() +
  ggtitle("Elasticity as a function of P") +
  xlab("P") + ylab("Elasticity")

p_e3

#browseURL("https://www.wolframalpha.com/input?i=simplify+-%2850*%28P%5E-%282%2B1%29*2%29%29+*+P%2F%2850*P%5E-2%29+")

#' A2 section

#' Convert the constant in the log-linear model to its original scale
log(exp(7.5))

#' Define the function for mortality as a function of income based on a log-log model
#' 1) log(MORTALITY) = 7.5 - 0.5* log(INCOME)
f1 <- makeFun(exp(7.5)*INCOME^-0.5 ~INCOME)  
#' Convert the function back to its original scale

#' Plot the mortality as a function of income without specific range limit
plotFun(f1(INCOME) ~ INCOME, main="mortality as a function of income")

#' Plot the mortality as a function of income for the range [0,30]
plotFun(f1(INCOME) ~ INCOME, xlim = c(0,30), main="mortality as a function of income")

#' Calculate the slope (first derivative) of the mortality function with respect to income
D(expression(exp(7.5)*INCOME^-0.5),"INCOME")  #' Derivative of the function

#' Calculate the elasticity of the mortality curve as (slope * INCOME) / MORTALITY
e1 <- function(INCOME) {-(exp(7.5) * (INCOME^-(0.5 + 1) * 0.5))*INCOME/f1(INCOME)}
e1(1)    #' Evaluate elasticity at INCOME=1
e1(0.5)  #' Evaluate elasticity at INCOME=0.5
e1(3)    #' Evaluate elasticity at INCOME=3

