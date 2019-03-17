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
### Katie's Section:
* Question: How many rides are there on each day of the week and which day has the highest percentage of rides?
* Findings: The 3 busiest days for rides are Saturday, Friday and Sunday. Saturday has 17.52% of all rides, Friday has 15.26% and Sunday has 14.73%. Weekends have the most amount of rides for 2017. 
  * Sunday: 67887 rides (14.73% of all rides in 2017)
  * Monday: 59408 rides (12.89% of all rides in 2017)
  * Tuesday: 61907 rides (13.44% of all rides in 2017)
  * Wednesday: 58931 rides (12.79% of all rides in 2017)
  * Thursday: 61556 rides (13.36% of all rides in 2017)
  * Friday: 70290 rides (15.26% of all rides in 2017)
  * Saturday: 80738 rides (17.52% of all rides in 2017)

```{r}
stations <- read.csv("Station_Locations.csv")
history <- read_csv("trip_history_2017_season.csv", col_types = cols(`Start date` = col_datetime(format = "%m/%d/%Y %H:%M"), `End date` = col_datetime(format = "%m/%d/%Y %H:%M"), `Total duration (Seconds)` = col_integer())) %>%
  rename(duration = `Total duration (Seconds)`) %>%
  mutate(minutes = duration %/% 60)
```
```{r}
start <- history %>%
  left_join(stations, c(`Start station` = "Name")) %>%
  rename(start_lat = Latitude, start_long = Longitude) %>%
  select(-Number) %>%
  mutate(case = row_number())
end <- history %>%
  left_join(stations, c(`End station` = "Name")) %>%
  rename(end_lat = Latitude, end_long = Longitude) %>%
  select(-Number) %>%
  mutate(case = row_number())
all <- start %>%
  inner_join(end, by = "case") %>%
  select(-25, -(14:22)) %>%
  mutate(day = wday(`Start date.x`, label = TRUE))
```
```{r}
count(all, day == "Sun")
count(all, day == "Mon")
count(all, day == "Tue") 
count(all, day == "Wed")
count(all, day == "Thu")
count(all, day == "Fri")
count(all, day == "Sat")
```

### Zandy's Section:
* Question: What are the busiiest bike routes depending on the day and the time of day?
* Findings: 

```{r}
Nice_Ride_2017_Station_Locations <- read_csv("C:/Users/zandy/Downloads/Nice_ride_data_2017_season/Nice_ride_data_2017_season/Nice_Ride_2017_Station_Locations.csv")
Parsed with column specification:
cols(
  Number = col_character(),
  Name = col_character(),
  Latitude = col_double(),
  Longitude = col_double(),
  `Total docks` = col_double()
)
> View(Nice_Ride_2017_Station_Locations)
> library(readr)
> Nice_ride_trip_history_2017_season <- read_csv("C:/Users/zandy/Downloads/Nice_ride_data_2017_season/Nice_ride_data_2017_season/Nice_ride_trip_history_2017_season.csv")
Parsed with column specification:
cols(
  `Start date` = col_character(),
  `Start station` = col_character(),
  `Start station number` = col_character(),
  `End date` = col_character(),
  `End station` = col_character(),
  `End station number` = col_character(),
  `Account type` = col_character(),
  `Total duration (Seconds)` = col_double()
)
> View(Nice_ride_trip_history_2017_season)



# Load libraries
library(tidyverse)
library(lubridate)


# plot Nice Ride Stations
ggplot() +
  geom_point(data=Nice_Ride_2017_Station_Locations, 
               aes(x=Longitude, y=Latitude), 
               color = 'blue', size = 1) +
  labs(x='Longitude', y='Latitude') +
  ggtitle('Locations of NiceRide Stations')

```

## Team Report:
* I, Kevin Luth, found the busiest three stations for each day of the week. I started by changing the column types for the start and end dates to the date-time format. I then used left_join() to get the latitude and longitudes into the same dataset as the one with the trip details for both the start and end locations. I mutated a column called case to serve as surrogate key to indicate the individual trips. I then used inner_join() to join the dataset with the starting coordinates to the one with the ending coordinates by the case key I created. I then mutated a column displaying the day of the week of each trip by using the wday() function. I then grouped by day and counted the uses of each station and arranged it in descending order by day, showing only the top 3 counts for each day using the top_n() function. Then I left joined the dataset with the coordinates to my new dataset and graphed the location of the most frequented stations using the coordinates. I also changed the color of each day's point and used facet_wrap() by the day to make it easy to tell where the locations were for each day.
