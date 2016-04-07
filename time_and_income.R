library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
# Read data
trip_data <- read_csv('201508_trip_data.csv.gz')
# Convert data columns
trip_data$`Start Date` <- parse_date_time(trip_data$`Start Date`, orders = c("mdyHM"))
trip_data$`End Date` <- parse_date_time(trip_data$`End Date`, orders = c("mdyHM"))
# Load census data and join it with the trip data
census_data <- read_csv('DEC_00_SF3_HCT012.csv.gz')
trips_with_demographics <- trip_data %>%
  inner_join(census_data, c("Zip Code" = "GEO.id2"))
# Clean up the data a little ...
income_by_start_hour <- trips_with_demographics %>%
  transmute(`Median Income`=Total,
            Hour = hour(`Start Date`) +
              minute(`Start Date`) / 60) %>%
  na.omit()
# And plot it.
ggplot(income_by_start_hour, aes(x=Hour, y=`Median Income`)) +
  geom_smooth()
ggsave("income_vs_time_smooth.png")
afternoon_trips <- income_by_start_hour %>%
  filter(Hour > 15, Hour < 21)
afternoon_model <- lm(`Median Income`~Hour, afternoon_trips)
print(summary(afternoon_model))
morning_trips <- income_by_start_hour %>%
  filter(Hour > 4, Hour < 10)
morning_model <- lm(`Median Income`~Hour, morning_trips)
print(summary(morning_model))
