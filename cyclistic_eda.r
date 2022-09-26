library(tidyverse)
library(sp)
library(rgdal)
library(ggplot2)

# q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")

#import and merge all three CSV files into one data frame
# Used the clean name function to make sure all columns were
# unique and had no title spacing.
csv_data <- list.files(path = "./data", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows %>%
  janitor::clean_names()


###########################
# Removed duplicate records with the use of unique ride ids.
# Deleted rows with unavailable or null data.
csv_data <- csv_data %>% distinct(ride_id, .keep_all = TRUE) %>% drop_na()

# Added a column for the duration of the trip in minutes
# Create a column called “day_of_week,” and calculate the day of the week that each ride started
# noting that 1 = Sunday and 7 = Saturday
csv_data <- csv_data %>%
  mutate(ride_length = ((as.integer(ended_at) - as.integer(started_at)) / 60))

csv_data$hour_of_start <- as.numeric(format(csv_data$started_at, "%H"))
csv_data$day_of_week <- lubridate::wday(csv_data$started_at)
csv_data$month_of_start <- as.numeric(format(csv_data$started_at, "%m"))


# Deleted rows with inconsistent times where the start time
# was later than the end time. Removed rows whos start station 
# was the same as the end station
csv_data <- subset(csv_data, ride_length > 0 & start_station_id != end_station_id )

# View(summary(preped_data))

# Manage outlier with an upper and lower bound
# With the percentiles method, all observations that lie outside the interval
# formed by the 2.5 and 99 percentiles will be considered as potential outliers.
lower_bound <- quantile(csv_data$ride_length, 0.01)
upper_bound <- quantile(csv_data$ride_length, 0.99)

# find the row number of the observation that is an outlier
outlier_ind <- which(csv_data$ride_length < lower_bound | csv_data$ride_length > upper_bound)

# use the outlier indication list to create a dataframe of outliers
quant_out <- csv_data[outlier_ind,
  c("ride_id", "member_casual", "ride_length")]

# look at the summary of the outliers
# View(summary(quant_out))

# remove the outliers from the working datafame
csv_data <- csv_data[-outlier_ind, ]


# write.csv(csv_data, ".//cyclistic_data_21-22.csv", row.names = FALSE)

View(summary(csv_data))

# View(head(processed_data))
# 4,787,188 observations left after processing and cleaning

#########################
# Average ride time per membership for the entire set
# View(aggregate(csv_data$ride_length ~ csv_data$member_casual,
#   FUN = mean))

# Average ride time per day of the week per membership
# View(aggregate(csv_data$ride_length ~ csv_data$member_casual +
#   csv_data$day_of_week, FUN = mean))

# # analyze ridership data by type and weekday
# View(csv_data %>%
#       group_by(member_casual, day_of_week) %>%
#       summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
#       arrange(member_casual, day_of_week))

# , "start_lng", "start_lat")



stations_member_data <- csv_data %>%
  filter(member_casual == "member") %>%
  group_by(end_station_name) %>%
  summarise(number_of_rides = n(),
    station_lng = median(start_lng), station_lat = median(start_lat)) %>%
  arrange(desc(number_of_rides)) %>%
  slice(0:10)

# write.csv(stations_member_data, ".//cyclistic_member_gis.csv", row.names = FALSE)

stations_casual_data <- csv_data %>%
  filter(member_casual == "casual") %>%
  group_by(end_station_name) %>%
  summarise(number_of_rides = n(),
    station_lng = median(start_lng), station_lat = median(start_lat)) %>%
  arrange(desc(number_of_rides)) %>%
  slice(0:10)

# write.csv(stations_casual_data, ".//cyclistic_casual_gis.csv", row.names = FALSE)

# station_member <- stations_member_data %>%
#   ggplot(aes(x = start_station_name, y = number_of_rides)) +
#     geom_col(position = "dodge") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# station_casual <- stations_casual_data %>%
#   ggplot(aes(x = start_station_name, y = number_of_rides)) +
#     geom_col(position = "dodge") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

View(stations_member_data)
# plot(station_member)

View(stations_casual_data)
# plot(station_casual)

# #######################
# coordinates(stations_member_data) <- c("station_lng", "station_lat")
# crs.geol <- CRS("+proj=longlat")
# proj4string(stations_member_data) <- crs.geol

# chicago <- readOGR(dsn = "./communities",
#   layer = "geo_export_f068a02e-5d33-4c10-a2e7-30a0729cdca2")

# plot(chicago)
# points(stations_member_data, pch = 0:21, col = "steelblue")
##########################


# avg_dot <- csv_data %>%
#   group_by(member_casual, month_of_start) %>%
#   summarise(average_duration = mean(ride_length)) %>%
#   ggplot(aes(x = month_of_start, y = average_duration, fill = member_casual)) +
#     geom_point(position = "dodge") +
#     scale_x_continuous(breaks = seq(1, 12, 1), limits = c(0.5, 12.5)) +
#     scale_y_continuous(labels = scales::comma) +
#     theme(text = element_text(size = 16))

# plot(avg_dot)

# Visualization for number of rides by rider type
# plot(processed_data %>%
#       group_by(member_casual, day_of_week) %>%
#       summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
#       arrange(member_casual, day_of_week) %>%
#       ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
#         geom_col(position = "stack"))

# # Visualization for average duration
# plot(processed_data %>%
#       group_by(member_casual, day_of_week) %>%
#       summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
#       arrange(member_casual, day_of_week) %>%
#       ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
#         geom_col(position = "stack"))