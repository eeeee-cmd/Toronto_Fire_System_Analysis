#### Preamble ####
# Purpose: Simulates data
# Author: Deyi Kong
# Date: 27 September 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(lubridate)

#### Simulate data ####
set.seed(12345)

# Define the start and end time of TFS responds
start_time <- as.POSIXct("2018-01-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2023-12-31 00:00:00", tz = "UTC")

# Set the number of random incidents want to generate
number_of_cases <- 100

# Generate random time stamps for tfs alarm notified
data <- tibble(
  tfs_alarm_time = as.POSIXct(
    runif(
      n = number_of_cases,
      min = as.numeric(start_time),
      max = as.numeric(end_time)
    ),
    origin = "1970-01-01"
  )
)

# Generate random time differences between alarm notified and tfs arrival time in second
notified_until_arrival <- as.integer(runif(number_of_cases, min = 120, max = 600))
# Calculate the arrival time by adding the time difference to alarm notified time
tfs_arrival_time <- as.POSIXct(data$tfs_alarm_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") + notified_until_arrival

# Simulate the estimated loss in dollars
estimated_dollar_loss <- sample(0:100000, number_of_cases, replace = TRUE)

# Simulate the most common categories for each categorical columns
area_of_origin <- sample(c(
  "24 - Cooking Area or Kitchen",
  "81 - Engine Area",
  "44 - Trash, Rubbish Storage",
  "64 - Porch or Balcony",
  "22 - Sleeping Area or Bedroom"
), number_of_cases, replace = TRUE)
# Simulate the most common categories for ignition source
ignition_source <- sample(c(
  "11 - Stove, Range-top burner",
  "71 - Smoker's Articles",
  "81 - Vehicle - Electrical",
  "82 - Vehicle - Mechanical"
), number_of_cases, replace = TRUE)
# Simulate the most common categories for extent of fire
extent_of_fire <- sample(c(
  "1 - Confined to object of origin",
  "2 - Confined to part of room/area of origin"
), number_of_cases, replace = TRUE)
# Simulate the most common categories for systems
fire_alarm_system <- sample(c("No System", "System Presents and Operated", "System Presents"), number_of_cases, replace = TRUE)
smoke_alarm_at_fire_origin <- sample(c("No System", "System Presents and Operated", "System Presents"), number_of_cases, replace = TRUE)
sprinkler_system <- sample(c("No System", "System Presents and Operated", "System Presents"), number_of_cases, replace = TRUE)

# Adding the variables into the data set
data <- 
  data |>
  mutate(
    tfs_arrival_time = tfs_arrival_time,
    notified_until_arrival = notified_until_arrival,
    area_of_origin = area_of_origin,
    estimated_dollar_loss = estimated_dollar_loss,
    extent_of_fire = extent_of_fire,
    fire_alarm_system = fire_alarm_system,
    ignition_source = ignition_source,
    smoke_alarm_at_fire_origin = smoke_alarm_at_fire_origin,
    sprinkler_system = sprinkler_system
)
#head(data)

# Format the alarm time without "T" and "Z"
data <- data |>
  mutate(
    tfs_alarm_time = format(tfs_alarm_time, "%Y-%m-%d %H:%M:%S"),
    tfs_arrival_time = format(tfs_arrival_time, "%Y-%m-%d %H:%M:%S"),
    )

#### Write_csv
write_csv(data, file = "data/raw_data/simulated.csv")


