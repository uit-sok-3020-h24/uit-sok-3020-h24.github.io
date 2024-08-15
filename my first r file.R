# this is a comment
2+2

#http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata

# URL of the .RData file
url_2 <- "http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"

# Any function in R looks like
# verb(x, ...)

# Load the file directly from the URL
load(url(url_2))

# cleaning up
rm(list=ls())

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))

names(tuna)
# Assuming 'tuna' is your data frame
names(tuna) <- c("q1", "p1", "p2", "p3", "disp", "dispad")

# Check the new names
names(tuna)

# Now the data should be loaded into your R environment. You can check by listing the objects in your environment.
ls()

mean(tuna$q1)

# storing the mean in an object, a scalar
q1_mean <- mean(tuna$q1)
mean_q1 <- mean(tuna$q1)

# plot p1 against q1

# Plotting p1 against q1
plot(tuna$p1, tuna$q1, main="Scatter plot of p1 vs q1",
     xlab="p1", ylab="q1", pch=19, col="blue")

# As an economist, I would like to have price on the y axis
plot(tuna$q1, tuna$p1, main="Scatter plot of p1 vs q1",
     xlab="p1", ylab="q1", pch=19, col="red")

# Install ggplot2 if it's not already installed
#if (!require(ggplot2)) install.packages("ggplot2")
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Create a scatter plot using ggplot2
plot1 <- ggplot(tuna, aes(x = q1, y = p1)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Scatter Plot of p1 vs q1", x = "q1", y = "p1") +
  theme_minimal()

plot1

# the correlation between price and quantity
cor(tuna$q1,tuna$p1)
cor(tuna$p1,tuna$q1)

cor(tuna$p1,tuna$p1)

# lets change the scale of prices from $ to cents
# there is a 100 cents in a dollar
# this is base r way of doing it
# tuna$p1 <- tuna$p1*100

# Install tidyverse if it's not already installed
#if (!require(tidyverse)) install.packages("tidyverse")

# Load the tidyverse package
library(tidyverse)

# Assuming 'p1' is the price column in dollars, and you want to convert it to cents
tuna2 <- 
  tuna %>%
  mutate(p1_cents = p1 * 100,
         p2_cents = p2 * 100,
         p3_cents = p3 * 100)  # Creates a new column p1_cents

# Now, plot p1_cents against q1 using ggplot2
tuna2 %>% 
ggplot(aes(x = q1, y=p1_cents)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of p1 (cents) vs q1", y = "p1 (cents)", x = "q1") +
  theme_minimal()

