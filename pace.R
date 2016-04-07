library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
# Read data
trip.data <- read_csv('201508_trip_data.csv.gz')
trips.with.pace <- trip.data %>% 
  group_by(`Start Station`, `End Station`) %>%
  transform(Pace=Duration/mean(Duration)) %>%
  filter(Pace < 3)
ggplot(trips.with.pace, aes(Pace, fill=Subscriber.Type)) +
  geom_histogram(position = 'fill')
ggsave("pace_hist.png")
pace <- trips.with.pace %>%
  group_by(Zip.Code) %>%
  summarize(`Average Pace` = mean(Pace), `Sd Pace` = sd(Pace), `Samples` = n()) %>%
  filter(`Sd Pace` < `Average Pace` * 0.5, Samples > 300) %>%
  arrange(`Average Pace`) %>%
  head(10)
ggplot(pace, aes(x=Zip.Code, y=`Average Pace`)) +
  geom_bar(stat='identity', fill='steelblue4', color='black') +
  geom_errorbar(aes(ymin=`Average Pace` - `Sd Pace`,
                    ymax=`Average Pace` + `Sd Pace`),
                width=.3)
ggsave("pace_vs_zip.png")
trips.with.pace %>%
  filter(Zip.Code == "94588") %>%
  group_by(Start.Station, End.Station) %>%
  summarize(Count=n(), `Average Pace`=mean(Pace)) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  head() %>%
  print()
