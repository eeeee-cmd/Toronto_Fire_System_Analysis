#### Preamble ####
# Purpose: Cleans the raw data of incidents into an analysis dataset
# Author: Deyi Kong
# Date: 27 September 2024
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

# # get the column names of raw_data
# colnames(raw_data)
# 
# #  check each variables' information for features selection
# summary(raw_data)
# 
# # check how many different types are the final incidents
# unique(raw_data$Area_of_Origin)
# # Get top 5 counts for area of origin
# top_five_origin <- 
#   raw_data |>
#   count(Area_of_Origin) |>
#   arrange(desc(n)) |>
#   top_n(5, n)
# # Print the result
# print(top_five_origin)
# unique(raw_data$Civilian_Casualties)
# unique(raw_data$Estimated_Dollar_Loss)
# unique(raw_data$Exposures)
# unique(raw_data$Extent_Of_Fire)
# # Get top 5 counts using dplyr
# top_five_extent <- 
#   raw_data |>
#   count(Extent_Of_Fire) |>
#   arrange(desc(n)) |>
#   top_n(5, n)
# # Print the result
# print(top_five_extent)
# unique(raw_data$Final_Incident_Type)
# unique(raw_data$Fire_Alarm_System_Operation)
# unique(raw_data$Fire_Alarm_System_Presence)
# sort(unique(raw_data$Ignition_Source))
# # Get top 5 counts using dplyr
# top_five_igsource <- 
#   raw_data |>
#   count(Ignition_Source) |>
#   arrange(desc(n)) |>
#   top_n(5, n)
# # Print the result
# print(top_five_igsource)
# unique(raw_data$TFS_Firefighter_Casualties)
# unique(raw_data$Method_Of_Fire_Control)
# # Get top 5 counts using dplyr
# top_five_firstig <- 
#   raw_data |>
#   count(Material_First_Ignited) |>
#   arrange(desc(n)) |>
#   top_n(5, n)
# # Print the result
# print(top_five_firstig)
# unique(raw_data$Smoke_Alarm_at_Fire_Origin)
# unique(raw_data$Sprinkler_System_Operation)
# unique(raw_data$Sprinkler_System_Presence)
# unique(raw_data$Status_of_Fire_On_Arrival)

# pre-data cleaning for future use
current_data <- 
  raw_data |>
  janitor::clean_names() |>
  # extract the year and month for each fire incidents
  mutate(
    year = year(as.Date(tfs_alarm_time)),
    month = month(as.Date(tfs_alarm_time))
  ) |>
  # replace NA in estimated dollar loss to 0
  mutate(
    stimated_dollar_loss = as.numeric(estimated_dollar_loss) |>
      replace_na(0)
  ) |>
  # replace NA in civilian casualties
  mutate(civilian_casualties = ifelse(is.na(civilian_casualties), 0, civilian_casualties))


# Calculate Q1 (25th percentile) and Q3 (75th percentile)
Q1 <- quantile(current_data$estimated_dollar_loss, 0.25, na.rm = TRUE)
Q3 <- quantile(current_data$estimated_dollar_loss, 0.75, na.rm = TRUE)

# Calculate IQR
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

cleaned_data <- 
  current_data |>
  # selecting the variables that will be used later
  select(year, month, area_of_origin, estimated_dollar_loss, civilian_casualties, extent_of_fire, fire_alarm_system_impact_on_evacuation, fire_alarm_system_operation, ignition_source, smoke_alarm_at_fire_origin, sprinkler_system_operation, status_of_fire_on_arrival, fire_under_control_time, tfs_alarm_time, tfs_arrival_time) |>
  drop_na() |>
  filter(estimated_dollar_loss >= lower_bound & estimated_dollar_loss <= upper_bound) |>
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
  # formating variables
  mutate(
    tfs_alarm_time = format(tfs_alarm_time, "%Y-%m-%d %H:%M:%S"),
    tfs_arrival_time = format(tfs_arrival_time, "%Y-%m-%d %H:%M:%S"),
    fire_under_control_time = format(fire_under_control_time, "%Y-%m-%d %H:%M:%S"),
  ) |>
  # replace those undetermined status to NA
  mutate(
    extent_of_fire = if_else(str_detect(extent_of_fire, "99 - Undetermined"), NA_character_, extent_of_fire),
    fire_alarm_system_impact_on_evacuation = if_else(str_detect(fire_alarm_system_impact_on_evacuation, "9 - Undetermined"), NA_character_, fire_alarm_system_impact_on_evacuation),
    fire_alarm_system_operation = if_else(str_detect(fire_alarm_system_operation, "9 - Fire alarm system operation undetermined"), NA_character_, fire_alarm_system_operation),
    ignition_source = if_else(str_detect(ignition_source, "999 - Undetermined | 9990 - Under Investigation" ), NA_character_, extent_of_fire),
    sprinkler_system_operation = if_else(str_detect(sprinkler_system_operation, "9 - Activation/operation undetermined" ), NA_character_, sprinkler_system_operation)
    ) |>
  mutate(
    fire_alarm_system = case_when(
      is.na(fire_alarm_system_operation) ~ "System Presents",
      fire_alarm_system_operation == "8 - Not applicable (no system)" ~ "No System",
      fire_alarm_system_operation == "1 - Fire alarm system operated" ~ "System Presents and Operated",
      fire_alarm_system_operation == "2 - Fire alarm system did not operate" ~ "System Presents",
      fire_alarm_system_operation == "9 - Fire alarm system operation undetermined" ~ "System Presents",
      TRUE ~ fire_alarm_system_operation
    ),
    smoke_alarm_at_fire_origin = case_when(
      is.na(smoke_alarm_at_fire_origin) ~ "System Presents",
      smoke_alarm_at_fire_origin == "9 - Floor/suite of fire origin: Smoke alarm presence undetermined" ~ "No System",
      smoke_alarm_at_fire_origin == "1 - Floor/suite of fire origin: No smoke alarm" ~ "No System",
      smoke_alarm_at_fire_origin == "2 - Floor/suite of fire origin: Smoke alarm present and operated" ~ "Presents and Operated",
      smoke_alarm_at_fire_origin == "3 - Floor/suite of fire origin: Smoke alarm present did not operate" ~ "System Presents",
      smoke_alarm_at_fire_origin == "4 - Floor/suite of fire origin: Smoke alarm present, operation undetermined" ~ "System Presents",
      smoke_alarm_at_fire_origin == "2 - Smoke alarm present and operated" ~ "Presents and Operated",
      smoke_alarm_at_fire_origin == "1 - No smoke alarm" ~ "No System",
      smoke_alarm_at_fire_origin == "3 - Smoke alarm present did not operate" ~ "System Presents",
      smoke_alarm_at_fire_origin == "4 - Smoke alarm present, operation undetermined" ~ "System Presents",
      sprinkler_system_operation == "9 - Smoke alarm presence undetermined" ~ "No System",
      TRUE ~ smoke_alarm_at_fire_origin
    ),
    sprinkler_system = case_when(
      is.na(sprinkler_system_operation) ~ "System Presents",
      sprinkler_system_operation == "8 - Not applicable - no sprinkler system present" ~ "No System",
      sprinkler_system_operation == "1 - Sprinkler system activated" ~ "Presents and Activated",
      sprinkler_system_operation == "2 - Did not activate: remote from fire" ~ "System Presents",
      sprinkler_system_operation == "3 - Did not activate: fire too small to trigger system" ~ "System Presents",
      sprinkler_system_operation == "9 - Activation/operation undetermined" ~ "System Presents",
      sprinkler_system_operation == "4 - Other reason for non activation/operation" ~ "System Presents",
      sprinkler_system_operation == "5 - Did not activate: reason unknown" ~ "System Presents",
      TRUE ~ sprinkler_system_operation
    )
  ) |>
      mutate(
        month = cut(month,
                    breaks = c(0, 3, 6, 9, 12, 13),
                    labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
                    right = FALSE) # Include the left interval but exclude the right
    ) |>
  # modify the columns for simplicity (some variables become redundant after we used as calculation/modification)
  select(year, month, area_of_origin, estimated_dollar_loss, civilian_casualties, extent_of_fire, ignition_source, fire_alarm_system, smoke_alarm_at_fire_origin, sprinkler_system, status_of_fire_on_arrival, fire_alarm_system_impact_on_evacuation)
  
 unique(cleaned_data$fire_alarm_system)
#summary(cleaned_data)

# # Identify which entries were converted to NA
# na_indices <- cleaned_data %>%
#   filter(is.na(fire_under_control_time))
# 
# na_indices_time <- cleaned_data %>%
#   filter(is.na(TFS_Notified_Till_Arrival))
#
# na_indices_dur <- cleaned_data %>%
#   filter(is.na(TFS_Duration))
# print(na_indices_dur)
# 
# na_indices_alarm <- cleaned_data |>
#   filter(is.na(fire_alarm_system))
# print(na_indices_alarm)

# incident_count_by_year <- cleaned_data %>%
#   group_by(year) %>%
#   summarize(incident_count = n())  # Count the number of incidents
# # Optionally, arrange the results by year
# incident_count_by_year <- incident_count_by_year %>%
#   arrange(year)
# # View the result
# print(incident_count_by_year)

#?difftime

#### Save data ####
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")

