# Parameters: Soil health, sunlight exposure, pest exposure, disease exposure, Genetic Makeup, Irrigation, Fertilization

# Load libraries
library(tidyverse)
library(here)
library(janitor)

# Load data
climate <- read_table(here("data", "clim.txt")) %>% 
  clean_names()


# Almond yield function
almond_yield2 <- function(climate) {
  
  # Group by year and month to get monthly averages
  almond_month <- climate %>% 
    group_by(year, month) %>% 
    summarise(tmin_avg = mean(tmin_c),  
              tmax_avg = mean(tmax_c),
              precip_total = sum(precip)) %>% # you don't want average precip, you want total for the month!
    ungroup()
  
  
  # Filter to january and february because those are the months used in the paper
  jan_data <- almond_month %>% 
    filter(month == 1) %>% 
    select(year, precip_total)
  
  feb_data <- almond_month %>% 
    filter(month == 2) %>% 
    select(year, tmin_avg)
  
  # Join just these two together by year
  clean_almond <- left_join(feb_data, jan_data, by = "year")
  
  # Fix the names
  names(clean_almond) <- c("year", "tmin", "precip")
  
  # Function that iterates over every observation
  almond_yield <- function(tmin, precip) {
    y <- (-0.015 * tmin - 0.0046 * tmin^2 - 0.07 * precip + 0.0043 * precip^2 + 0.28)
    return(y)
  }
  
  # Create yield column with calculated yield
  clean_almond$yield <- with(clean_almond, almond_yield(tmin, precip))
  
  # Summarize data
  summary(clean_almond$yield)
}

almond_yield2(climate)
