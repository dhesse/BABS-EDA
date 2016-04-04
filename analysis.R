library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
# Read data
trip_data <- read_csv('201508_trip_data.csv.gz')
# Convert data columns
trip_data$`Start Date` <- parse_date_time(trip_data$`Start Date`, orders = c("mdyHM"))
trip_data$`End Date` <- parse_date_time(trip_data$`End Date`, orders = c("mdyHM"))
# Find and plot top 20 stations, by trip starts
top_twenty <- trip_data %>%
  group_by(`Start Station`) %>%
  summarize(`Trip Count` = n()) %>%
  arrange(desc(`Trip Count`)) %>%
  head(20)
ggplot(data=top_twenty, aes(x=`Start Station`, y=`Trip Count`)) +
  geom_bar(stat="identity", fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('most_popular_stations.png')
# Plot trips from most popular station, by hour of the day
most_popular_station <- top_twenty$`Start Station`[1]
from_most_popular <- trip_data %>%
  filter(`Start Station` == most_popular_station) %>%
  mutate(`Start Hour` = hour(`Start Date`))
ggplot(data=from_most_popular, aes(x=`Start Hour`)) +
  geom_histogram(binwidth=1, fill='steelblue') +
  ggtitle(paste('Trips from', most_popular_station))
ggsave('time_of_trips.png')
# Find popular destinations, before and after noon
early_end_stations <- from_most_popular %>%
  filter(`Start Hour` < 12) %>%
  group_by(`End Station`) %>%
  summarize(`Trip Count` = n()) %>%
  arrange(desc(`Trip Count`)) %>%
  head(20)
ggplot(data=early_end_stations, aes(x=`End Station`, y=`Trip Count`)) +
  geom_bar(stat="identity", fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('End Stations, Before Noon')
ggsave('early_end_stations.png')
late_end_stations <- from_most_popular %>%
  filter(`Start Hour` >= 12) %>%
  group_by(`End Station`) %>%
  summarize(`Trip Count` = n()) %>%
  arrange(desc(`Trip Count`)) %>%
  head(20)
ggplot(data=late_end_stations, aes(x=`End Station`, y=`Trip Count`)) +
  geom_bar(stat="identity", fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('End Stations, After Noon')
ggsave('late_end_stations.png')