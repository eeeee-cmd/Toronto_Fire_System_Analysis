#### Preamble ####
# Purpose: Cleans the raw data of incidents into an analysis dataset
# Author: Deyi Kong
# Date: 23 September 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None.

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_data.csv")

#colnames(raw_data)
summary(raw_data)
typeof(raw_data$TFS_Alarm_Time)
#unique(raw_data$Final_Incident_Type)
na_indices_control <- raw_data %>%
  filter(is.na(Fire_Under_Control_Time) & !is.na(Status_of_Fire_On_Arrival))
summary(na_indices_control)

cleaned_data <- 
  raw_data |>
  janitor::clean_names() |>
  select(area_of_origin, estimated_dollar_loss, final_incident_type, fire_alarm_system_impact_on_evacuation, fire_alarm_system_presence, ignition_source, smoke_alarm_impact_on_persons_evacuating_impact_on_evacuation, sprinkler_system_presence, status_of_fire_on_arrival, fire_under_control_time, tfs_alarm_time, tfs_arrival_time) |>

  drop_na() |>
  filter(!is.na(status_of_fire_on_arrival) & !is.na(fire_under_control_time)) |>
  # Calculate duration based on the parsed date-time
  mutate(
    TFS_Notified_Till_Arrival = as.numeric(difftime(tfs_arrival_time, tfs_alarm_time, units = "secs")),
    TFS_Duration = as.numeric(difftime(fire_under_control_time, tfs_arrival_time, units = "secs"))
  )|>  
  # Parse the date-time strings directly
  mutate(
    tfs_alarm_time = parse_date_time(tfs_alarm_time, orders = c("ymd HMS", "ymd HMS"), quiet = TRUE),
    tfs_arrival_time = parse_date_time(tfs_arrival_time, orders = c("ymd HMS", "ymd HMS"), quiet = TRUE),
    fire_under_control_time = parse_date_time(fire_under_control_time, "ymd HMS", quiet = TRUE)
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
summary(cleaned_data)
# Identify which entries were converted to NA

na_indices <- cleaned_data %>%
  filter(is.na(fire_under_control_time))
print(na_indices)

na_indices_dur <- cleaned_data %>%
  filter(is.na(TFS_Duration))
print(na_indices_dur)

?difftime

#### Save data ####
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")

