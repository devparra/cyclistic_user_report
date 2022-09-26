# Extract Transform and Load of Cyclistic data
# Google Data Analytics Capstone
#
# Library for cleaning
library(tidyverse)

# import and merge all three CSV files into one data frame
# Used the clean name function to make sure all columns were
# unique and had no title spacing.
csv_data <- list.files(path = "./data", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows %>%
  janitor::clean_names()

# Removed duplicate records with the use of unique ride ids.
# Deleted rows with unavailable or null data.
csv_data <- csv_data %>% distinct(ride_id, .keep_all = TRUE) %>% drop_na()

# Added a column for the duration of the trip in minutes
# Create a column called “day_of_week,” and calculate the day of the week
# that each ride started
# noting that 1 = Sunday and 7 = Saturday
csv_data <- csv_data %>%
  mutate(ride_length = ((as.integer(ended_at) - as.integer(started_at)) / 60))

csv_data$hour_of_start <- as.numeric(format(csv_data$started_at, "%H"))
csv_data$day_of_week <- lubridate::wday(csv_data$started_at)
csv_data$month_of_start <- as.numeric(format(csv_data$started_at, "%m"))


# Deleted rows with inconsistent times where the start time
# was later than the end time. Removed rows whos start station 
# was the same as the end station
csv_data <- subset(csv_data, ride_length > 0 & 
  start_station_id != end_station_id )


# Manage outlier with an upper and lower bound
# With the percentiles method, all observations that lie outside the interval
# formed by the 2.5 and 99 percentiles will be considered as potential outliers.
lower_bound <- quantile(csv_data$ride_length, 0.01)
upper_bound <- quantile(csv_data$ride_length, 0.99)

# find the row number of the observation that is an outlier
outlier_ind <- which(csv_data$ride_length < lower_bound |
  csv_data$ride_length > upper_bound)

# use the outlier indication list to create a dataframe of outliers
quant_out <- csv_data[outlier_ind,
  c("ride_id", "member_casual", "ride_length")]

# remove the outliers from the working datafame
csv_data <- csv_data[-outlier_ind, ]

# write the csv data to a complete file
write.csv(csv_data, ".//cyclistic_data_21-22.csv", row.names = FALSE)

View(summary(csv_data))