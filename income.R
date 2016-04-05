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
# Calculate per-destination mean income
# NB: 'mean' is the mean of the median household income
trips_by_income <- trips_with_demographics %>%
  filter(!is.na(`Total`)) %>%
  group_by(`End Station`) %>%
  summarize(`Mean Income` = mean(Total),
            `Trip Count` = n()) %>%
  arrange(desc(`Mean Income`))
# Calculate and plot where people from areas
# with high/low median household income go
rich_destinations <-  trips_by_income %>%
  head(20)
poor_destinations <- trips_by_income %>%
  tail(20)
ggplot(rich_destinations, aes(x=`End Station`, y=`Mean Income`)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Trip Destinations, High Income')
ggsave('high_income_destinations.png')
ggplot(poor_destinations, aes(x=`End Station`, y=`Mean Income`)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Trip Destinations, Low Income')
ggsave('high_income_destinations.png')
# Extract and plot zip codes of people goint to Grant/Columbus
grant_ave_zips <- trip_data %>% 
  filter(`End Station` == "Grant Avenue at Columbus Avenue") %>%
  group_by(`Zip Code`) %>%
  summarise(`Trip Count` = n()) %>%
  arrange(desc(`Trip Count`)) %>%
  head(10)
ggplot(grant_ave_zips, aes(x=`Zip Code`, y=`Trip Count`)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Grant Avenue Arrivals, by Zip Code')
ggsave('grant_ave_arrivals.png')
# Let's now look at the early trips, which should end where
# people work or switch to different means of transportation ...
early_trips_top_income <- trips_with_demographics %>%
  filter(hour(`End Date`) < 11) %>%
  group_by(`End Station`) %>%
  summarize(`Mean Income` = mean(Total),
            `Trip Count` = n()) %>%
  arrange(desc(`Mean Income`)) %>%
  filter(!is.na(`Mean Income`)) %>%
  head(10)
ggplot(early_trips_top_income, aes(x=`End Station`, y=`Mean Income`)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Popular Destinations Before 11am, High Income")
ggsave("popular_early_high_income.png")
# Do early risers earn more? Let's find out!
trips_for_plot <- trips_with_demographics %>%
  filter(hour(`Start Date`) >= 5, !is.na(Total)) %>%
  transmute(Income = Total, Hour=hour(`Start Date`) + round(minute(`Start Date`) / 10) / 6) %>%
  group_by(Hour) %>%
  summarize(`Mean Income`=mean(Income), `Trip Count`=n())
ggplot(trips_for_plot, aes(x=`Hour`, y=`Mean Income`)) +
  geom_path(colour='steelblue') +
  ggtitle("Mean Income vs. Trip Time")
ggsave("time_vs_income.png")