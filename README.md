# Lab-9-StatsSloths

## Team Section



## Individual Section

### Kevin's Section:
* Question: Where are the locations of the three busiest bike stations for each day of the week?

* Findings: The data I collected shows that the most commonly used stations are located in Northeast Minneapolis especially on weekdays (Mon-Fri). 

```{r}
locations <- read_csv("Nice_Ride_2017_Station_Locations.csv")
trip <- read_csv("Nice_ride_trip_history_2017_season.csv", col_types = cols(`Start date` = col_datetime(format = "%m/%d/%Y %H:%M"), `End date` = col_datetime(format = "%m/%d/%Y %H:%M"), `Total duration (Seconds)` = col_integer())) %>%
  rename(duration = `Total duration (Seconds)`) %>%
  mutate(minutes = duration %/% 60)

start_joined <- trip %>%
  left_join(locations, c(`Start station` = "Name")) %>%
  rename(start_lat = Latitude, start_long = Longitude) %>%
  select(-Number) %>%
  mutate(case = row_number())
end_joined <- trip %>%
  left_join(locations, c(`End station` = "Name")) %>%
  rename(end_lat = Latitude, end_long = Longitude) %>%
  select(-Number) %>%
  mutate(case = row_number())
all_joined <- start_joined %>%
  inner_join(end_joined, by = "case") %>%
  select(-25, -(14:22)) %>%
  mutate(day = wday(`Start date.x`, label = TRUE))
  
cases <- all_joined %>%
  group_by(day) %>%
  count(`Start station.x`) %>%
  group_by(day) %>%
  arrange(desc(n)) %>%
  group_by(day) %>%
  top_n(3) %>%
  arrange(day)
case_location <- cases %>%
  left_join(locations, c(`Start station.x` = "Name")) %>%
  arrange(day)

ggplot(data=case_location) +
  geom_jitter(mapping = aes(x=Longitude, y=Latitude, color = as.factor(day)), size = 2) +
  facet_wrap(~ day) +
  ggtitle('Locations of Most Used Stations by Day') +
  scale_color_discrete(name = "Weekday")
```
