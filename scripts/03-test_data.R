#### Preamble ####
# Purpose: Sanity check of the data
# Author: Deyi Kong
# Date: 27 September 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have simulated data
# Any other information needed? None.


#### Workspace setup ####
library(tidyverse)


#### Test data ####
data <- read_csv("data/raw_data/simulated.csv")

# Test for negative numbers
data$estimated_dollar_loss |> min() <= 0

# Test for NAs
all(is.na(data$estimated_dollar_loss))
all(is.na(data$fire_alarm_system))
