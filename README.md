# Lab-9-StatsSloths

## Team Section



## Individual Section

### Kevin's Section:
* Question: Where are the locations of the three busiest bike stations for each day of the week?

* Findings: The data I collected shows that the most commonly used stations are located in Northeast Minneapolis, especially on weekdays (Mon-Fri). The Coffman Union and Washington Ave SE & Union Street SE stations are used often during the week and the Lake Street & Knox Ave S station is used often on the weekend.

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



## Team Report:
* I, Kevin Luth, found the busiest three stations for each day of the week. I started by changing the column types for the start and end dates to the date-time format. I then used left_join() to get the latitude and longitudes into the same dataset as the one with the trip details for both the start and end locations. I mutated a column called case to serve as surrogate key to indicate the individual trips. I then used inner_join() to join the dataset with the starting coordinates to the one with the ending coordinates by the case key I created. I then mutated a column displaying the day of the week of each trip by using the wday() function. I then grouped by day and counted the uses of each station and arranged it in descending order by day, showing only the top 3 counts for each day using the top_n() function. Then I left joined the dataset with the coordinates to my new dataset and graphed the location of the most frequented stations using the coordinates. I changed the color of each day's point and used facet_wrap() by the day to make it easy to tell where the locations were for each day.
