#### Preamble ####
# Purpose: Cleans the raw data of incidents into an analysis dataset
# Author: Deyi Kong
# Date: 25 September 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None.

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_data.csv")

# get the column names of raw_data
colnames(raw_data)

#  check each variables' information for features selection
summary(raw_data)

# check how many different types are the final incidents
unique(raw_data$Area_of_Origin)
unique(raw_data$Building_Status)
unique(raw_data$Business_Impact)
unique(raw_data$Extent_Of_Fire)
unique(raw_data$Final_Incident_Type)
unique(raw_data$Fire_Alarm_System_Impact_on_Evacuation)
unique(raw_data$Fire_Alarm_System_Operation)
unique(raw_data$Fire_Alarm_System_Presence)
levels(raw_data$Ignition_Source)

# filter the data from 2018 to 2023 because that was the time period we care about
current_data <- 
  raw_data |>
  mutate(
    year = year(as.Date(TFS_Alarm_Time))
  ) |>
  filter((year >= 2018 & year <= 2023))

# old_data <- 
#   raw_data |>
#   mutate(
#     year = year(as.Date(TFS_Alarm_Time))
#   ) |>
#   filter(!(year >= 2018 & year <= 2023))
# 
# summary(old_data)
# summary(current_data)

cleaned_data <- 
  current_data |>
  janitor::clean_names() |>
  # selecting the variables that will be used later
  select(year, area_of_origin, estimated_dollar_loss, fire_alarm_system_impact_on_evacuation, fire_alarm_system_presence, ignition_source, smoke_alarm_impact_on_persons_evacuating_impact_on_evacuation, sprinkler_system_presence, status_of_fire_on_arrival, fire_under_control_time, tfs_alarm_time, tfs_arrival_time) |>
  drop_na() |>
  filter(!is.na(status_of_fire_on_arrival) & !is.na(fire_under_control_time)) |>
  # Calculate duration and time spent for TFS to arrive based on the parsed date-time
  mutate(
    TFS_Notified_Till_Arrival = as.numeric(difftime(tfs_arrival_time, tfs_alarm_time, units = "secs")),
    TFS_Duration = as.numeric(difftime(fire_under_control_time, tfs_arrival_time, units = "secs")),
  )|>  
  # Parse the date-time strings directly
  mutate(
    tfs_alarm_time = parse_date_time(tfs_alarm_time, orders = c("ymd HMS", "ymd HMS"), quiet = TRUE),
    tfs_arrival_time = parse_date_time(tfs_arrival_time, orders = c("ymd HMS", "ymd HMS"), quiet = TRUE),
    fire_under_control_time = parse_date_time(fire_under_control_time, orders = c("ymd HMS", "ymd HMS"), quiet = TRUE),
    # If fire_under_control_time is NA, set it to "00:00:00"
    fire_under_control_time = coalesce(fire_under_control_time, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
    ) |>
  # Extract YMD and format HMS
  mutate(
    estimated_dollar_loss = as.numeric(estimated_dollar_loss),
    alarm_date = format(tfs_alarm_time, "%Y-%m-%d"),
    arrive_date = format(tfs_arrival_time, "%Y-%m-%d"),
    control_date = format(fire_under_control_time, "%Y-%m-%d"),
    tfs_alarm_time = format(tfs_alarm_time, "%H:%M:%S"),
    tfs_arrival_time = format(tfs_arrival_time, "%H:%M:%S"),
    fire_under_control_time = format(fire_under_control_time, "%H:%M:%S"),
  )
#summary(cleaned_data)
# Identify which entries were converted to NA

# na_indices <- cleaned_data %>%
#   filter(is.na(fire_under_control_time))
# 
# na_indices_time <- cleaned_data %>%
#   filter(is.na(TFS_Notified_Till_Arrival))
#
# na_indices_dur <- cleaned_data %>%
#   filter(is.na(TFS_Duration))
# print(na_indices_dur)

#?difftime

#### Save data ####
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")

