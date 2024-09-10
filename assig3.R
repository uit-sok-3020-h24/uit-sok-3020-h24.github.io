library(readxl)
library(lubridate)
library(tidyverse)

unemp <- read_excel("C:/Users/omy000/Downloads/unemployed_SSB_edit.xlsx", 
                                  range = "D3:E225")

gdp <- read_excel("C:/Users/omy000/Downloads/gdp_SSB_edit.xlsx", 
                           range = "B3:C102")

# Example data
dates <- c("2006M01", "2007M05", "2008M12")

# Replace 'M' with '-' to make it a standard format
dates <- gsub("M", "-", dates)

# Parse the dates using lubridate's ym() function
formatted_dates <- ym(dates)

# Print the formatted dates
str(formatted_dates)

# Change the date variable in both datasets, using tidyverse
unemp <- 
  unemp %>% 
  mutate(date = ym(gsub("M", "-", date)))

gdp <- 
  gdp %>% 
  mutate(date = ym(gsub("M", "-", date)))

str(unemp)
str(gdp)

# Merge the datasets using inner join
merged_data <- 
  unemp %>% 
  inner_join(gdp, by = "date")

# create a percentage change in gdp using tidyverse
merged_data <- 
  merged_data %>% 
  mutate(gdp_change = (gdp - lag(gdp)) / lag(gdp) * 100,
         unemp_change = (unemp - lag(unemp)) / lag(unemp) * 100)

# make a histogram of the percentage change in gdp
ggplot(merged_data, aes(x = gdp_change)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of GDP Change",
       x = "Percentage Change in GDP",
       y = "Frequency")

# make a histogram of the percentage change in unemp
ggplot(merged_data, aes(x = unemp_change)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Histogram of Unemployment Change",
       x = "Percentage Change in Unemployment",
       y = "Frequency")

