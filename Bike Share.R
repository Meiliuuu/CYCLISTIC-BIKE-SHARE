library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("/Users/meiliu/Desktop/Data/Excel Project/Cyclistic")

# Upload Divvy datasets (csv files) here
q1_2022 <- read_csv("Divvy_Trips_2022_Q1.csv")
q2_2022 <- read_csv("Divvy_Trips_2022_Q2.csv")
q3_2022 <- read_csv("Divvy_Trips_2022_Q3.csv")
q4_2022 <- read_csv("Divvy_Trips_2022_Q4.csv")

# Data Cleaning
# Compare column names each of the files
colnames(q1_2022)
colnames(q2_2022)
colnames(q3_2022)
colnames(q4_2022)

# Inspect the dataframes and look for incongruencies
str(q1_2022)
str(q2_2022)
str(q3_2022)
str(q4_2022)

# Convert started_at and ended_at to datetime so that they can stack correctly
q1_2022 <- q1_2022 %>%
  mutate(started_at = as.POSIXct(started_at, format = "%m/%d/%Y %H:%M:%S"),
         ended_at = as.POSIXct(ended_at, format = "%m/%d/%Y %H:%M:%S"))

# Merge all data
all_trips <- bind_rows(q1_2022, q2_2022, q3_2022, q4_2022)

# Remove fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new table that has been created
colnames(all_trips)
nrow(all_trips)
dim(all_trips)  
head(all_trips)
str(all_trips) 
summary(all_trips)

# seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))

# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove bad data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# DESCRIPTIVE ANALYSIS
# Number of Members are more than Casual riders
table(all_trips$member_casual)

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Reorder days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Average ride time by each day for members vs casual users
ride_length_agg <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)

# Visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  drop_na(weekday) %>% # Remove rows with NA values in the "weekday" column
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualize average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  drop_na(weekday) %>% # Remove rows with NA values in the "weekday" column
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Data/Excel Project/avg_ride_length.csv')




