# --- 1. SETUP: Install and Load Libraries ---

# Install packages if you haven't already (you only need to run these lines ONCE per R installation)
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")

# Load packages (you need to run these lines every time you start a new R session)
library(readxl)
library(dplyr)
library(lubridate)

# --- End SETUP ---

# --- 2. LOAD DATA ---

# Load 2019 Q1 data
df_2019 <- read_excel("Divvy_Trips_2019_Q1.xlsx")

# Load 2020 Q1 data
df_2020 <- read_excel("Divvy_Trips_2020_Q1.xlsx")

# --- End LOAD DATA ---

# --- 3. CLEANING STEP 1: Standardize Column Names ---

# For df_2019: Rename columns to match the 2020 schema
# We are creating a new data frame called df_2019_cleaned to keep the original safe
df_2019_cleaned <- df_2019 %>%
  rename(
    ride_id = trip_id,             # Renames 'trip_id' to 'ride_id'
    started_at = start_time,       # Renames 'start_time' to 'started_at'
    ended_at = end_time,           # Renames 'end_time' to 'ended_at'
    start_station_name = from_station_name, # Renames 'from_station_name'
    start_station_id = from_station_id,     # Renames 'from_station_id'
    end_station_name = to_station_name,     # Renames 'to_station_name'
    end_station_id = to_station_id          # Renames 'to_station_id'
  )

# For df_2020: We will also create a cleaned version for consistency,
# even though its column names are already mostly the target.
# This just ensures we're working with "cleaned" versions of both.
df_2020_cleaned <- df_2020

# --- End CLEANING STEP 1 ---

# --- 4. CLEANING STEP 2: Standardize User Type Values ---

# For df_2019_cleaned: Map 'Subscriber' to 'member' and 'Customer' to 'casual'
df_2019_cleaned <- df_2019_cleaned %>%
  mutate(
    member_casual = case_when(
      usertype == "Subscriber" ~ "member",  # If usertype is 'Subscriber', make it 'member'
      usertype == "Customer" ~ "casual",     # If usertype is 'Customer', make it 'casual'
      TRUE ~ as.character(usertype)          # Catches any unexpected values (unlikely here), keeps them as is (as character)
    )
  ) %>%
  select(-usertype) # Remove the old 'usertype' column, as we've created 'member_casual'

# For df_2020_cleaned: The 'member_casual' column already uses desired values, so no change is needed here.
# We will keep df_2020_cleaned as is for this step.

# --- End CLEANING STEP 2 ---

# --- 5. CLEANING STEP 3: Convert Date/Time Data Types ---

# Convert 'started_at' and 'ended_at' to proper datetime objects for both dataframes
# We use ymd_hms() from lubridate, assuming your times are in Year-Month-Day Hour:Minute:Second format.

df_2019_cleaned <- df_2019_cleaned %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )

df_2020_cleaned <- df_2020_cleaned %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )

# --- End CLEANING STEP 3 ---

# --- 6. CLEANING STEP 4: Calculate trip_duration for 2020 data ---

# For df_2019_cleaned: Rename the existing tripduration column for consistency
# We'll assume the 2019 'tripduration' column is already in seconds.
df_2019_cleaned <- df_2019_cleaned %>%
  rename(trip_duration = tripduration)

# For df_2020_cleaned: Calculate trip_duration in seconds
df_2020_cleaned <- df_2020_cleaned %>%
  mutate(trip_duration = as.numeric(difftime(ended_at, started_at, units = "secs")))

# --- End CLEANING STEP 4 ---

# --- 7. CLEANING STEP 5 & 6: Handle Missing Values & Filter Erroneous/Irrelevant Data ---

# Filter out rides with problematic durations:
# 1. Remove rows where trip_duration is missing (NA)
# 2. Remove rows where trip_duration is less than or equal to 60 seconds (0 or negative, and very short rides)
df_2019_cleaned <- df_2019_cleaned %>%
  filter(!is.na(trip_duration) & trip_duration > 60)

df_2020_cleaned <- df_2020_cleaned %>%
  filter(!is.na(trip_duration) & trip_duration > 60)

# --- End CLEANING STEP 5 & 6 (Part 1) ---

# --- 8. CLEANING STEP 5 & 6: Part 2: Missing Station Data & Removing Lat/Long ---

# Filter out rows with missing station names (start or end)
df_2019_cleaned <- df_2019_cleaned %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

df_2020_cleaned <- df_2020_cleaned %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

# Remove latitude and longitude columns as they are not consistent/needed for this analysis
df_2019_cleaned <- df_2019_cleaned %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

df_2020_cleaned <- df_2020_cleaned %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

# --- End CLEANING STEP 5 & 6 (Part 2) ---

# --- 9. CLEANING STEP 7: Combine the Data ---

# Combine the two cleaned dataframes into one large dataframe
# We use bind_rows() from dplyr, which stacks dataframes on top of each other.
# It automatically handles columns that are present in one but not the other (filling with NA).
combined_trips_df <- bind_rows(df_2019_cleaned, df_2020_cleaned)

# --- End CLEANING STEP 7 ---
# --- FIX: Ensure ride_id data type consistency for combining ---

# Convert ride_id in df_2019_cleaned to character type
df_2019_cleaned <- df_2019_cleaned %>%
  mutate(ride_id = as.character(ride_id))

# --- End FIX ---

# --- 9. CLEANING STEP 7: Combine the Data ---
combined_trips_df <- bind_rows(df_2019_cleaned, df_2020_cleaned)
# --- End CLEANING STEP 7 ---

# --- 10. ANALYSIS: Add Time-Based Columns ---

combined_trips_df <- combined_trips_df %>%
  mutate(
    ride_month = month(started_at, label = TRUE, abbr = FALSE),
    ride_day_of_week = wday(started_at, label = TRUE, abbr = FALSE), # Full weekday name
    ride_hour = hour(started_at)
  )

# --- End ANALYSIS: Add Time-Based Columns ---

# --- 11. ANALYSIS: Frequency and Temporal Patterns ---

# A. Total Ride Counts by User Type
total_rides_by_usertype <- combined_trips_df %>%
  group_by(member_casual) %>%
  summarize(total_rides = n()) # n() counts the number of rows in each group

# B. Ride Counts by Day of Week and User Type
rides_by_day_usertype <- combined_trips_df %>%
  group_by(member_casual, ride_day_of_week) %>%
  summarize(count = n()) %>%
  arrange(member_casual, ride_day_of_week) # Order results for easier viewing

# C. Ride Counts by Hour of Day and User Type
rides_by_hour_usertype <- combined_trips_df %>%
  group_by(member_casual, ride_hour) %>%
  summarize(count = n()) %>%
  arrange(member_casual, ride_hour)

# D. Ride Counts by Month and User Type
rides_by_month_usertype <- combined_trips_df %>%
  group_by(member_casual, ride_month) %>%
  summarize(count = n()) %>%
  arrange(member_casual, ride_month)

# --- End ANALYSIS: Frequency and Temporal Patterns ---

# --- 12. ANALYSIS: Average Trip Duration ---

# Calculate average trip duration by user type
average_duration_by_usertype <- combined_trips_df %>%
  group_by(member_casual) %>%
  summarize(avg_duration_seconds = mean(trip_duration))

# --- End ANALYSIS: Average Trip Duration ---

# --- 13. ANALYSIS: Geographical Patterns (Top Stations) ---

# A. Top 10 Most Popular Start Stations by User Type
top_start_stations <- combined_trips_df %>%
  group_by(member_casual, start_station_name) %>%
  summarize(ride_count = n()) %>%
  arrange(member_casual, desc(ride_count)) %>% # Sort by count in descending order
  slice_head(n = 10, by = member_casual) # Get top 10 for each member_casual group

# B. Top 10 Most Popular End Stations by User Type
top_end_stations <- combined_trips_df %>%
  group_by(member_casual, end_station_name) %>%
  summarize(ride_count = n()) %>%
  arrange(member_casual, desc(ride_count)) %>%
  slice_head(n = 10, by = member_casual)

# --- End ANALYSIS: Geographical Patterns ---

# --- 13. ANALYSIS: Geographical Patterns (Top Stations) - CORRECTED CODE ---

# A. Top 10 Most Popular Start Stations by User Type
top_start_stations <- combined_trips_df %>%
  group_by(member_casual, start_station_name) %>%
  summarize(ride_count = n()) %>%
  arrange(member_casual, desc(ride_count)) %>%
  slice_head(n = 10) # Removed 'by = member_casual'

# B. Top 10 Most Popular End Stations by User Type
top_end_stations <- combined_trips_df %>%
  group_by(member_casual, end_station_name) %>%
  summarize(ride_count = n()) %>%
  arrange(member_casual, desc(ride_count)) %>%
  slice_head(n = 10) # Removed 'by = member_casual'

# --- End ANALYSIS: Geographical Patterns (CORRECTED) ---

# --- 14. ANALYSIS: Preferred Bike Types ---

# Calculate bike type preference by user type
bike_type_preference <- combined_trips_df %>%
  group_by(member_casual, rideable_type) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% # Calculate percentage within each user group
  arrange(member_casual, desc(count))

# --- End ANALYSIS: Preferred Bike Types ---
table(df_2020_cleaned$rideable_type)
