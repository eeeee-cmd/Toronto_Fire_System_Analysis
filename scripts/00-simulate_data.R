#### Preamble ####
# Purpose: Simulates data
# Author: Deyi Kong
# Date: 23 September 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(lubridate)

#### Simulate data ####
set.seed(123)

# Define the start and end time of TFS responds
start_time <- as.Date("2018-01-01T00:00:00")
end_time <- as.Date("2023-12-31T00:00:00")

# Set the number of random incidents want to generate
number_of_cases <- 100

data <-
  tibble(
    time = as.Date(
      runif(
        n = number_of_cases,
        min = as.numeric(start_time),
        max = as.numeric(end_time)
      ),
      origin = "1970-01-01 00:00:00"
    ),
    estimated_dollar_loss = rpois(n = number_of_cases, lambda = 100)
  )


#### Write_csv
write_csv(data, file = "data/raw_data/simulated.csv")


