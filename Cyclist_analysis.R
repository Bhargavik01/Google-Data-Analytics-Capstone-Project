# install required packs 
install.packages("tidyverse")
install.packages("plotly")
install.packages("lubridate")
install.packages("ggplot2")

# calling packs i,e importing packages into library
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(readr)

# library(readr)
ds1 <- read_csv("Desktop/Bhargavi/dataset/Data/202108-divvy-tripdata.csv")
ds2 <- read_csv("Desktop/Bhargavi/dataset/Data/202109-divvy-tripdata.csv")
ds3 <- read_csv("Desktop/Bhargavi/dataset/Data/202110-divvy-tripdata.csv")
ds4 <- read_csv("Desktop/Bhargavi/dataset/Data/202111-divvy-tripdata.csv")
ds5 <- read_csv("Desktop/Bhargavi/dataset/Data/202112-divvy-tripdata.csv")
ds6 <- read_csv("Desktop/Bhargavi/dataset/Data/202201-divvy-tripdata.csv")
ds7 <- read_csv("Desktop/Bhargavi/dataset/Data/202202-divvy-tripdata.csv")
ds8 <- read_csv("Desktop/Bhargavi/dataset/Data/202203-divvy-tripdata.csv")
ds9 <- read_csv("Desktop/Bhargavi/dataset/Data/202204-divvy-tripdata.csv")
ds10 <- read_csv("Desktop/Bhargavi/dataset/Data/202205-divvy-tripdata.csv")
ds11 <- read_csv("Desktop/Bhargavi/dataset/Data/202206-divvy-tripdata.csv")
ds12 <- read_csv("Desktop/Bhargavi/dataset/Data/202207-divvy-tripdata.csv")

# inspect the dataframes to check  consistency
str(ds1)
str(ds2)
str(ds3)
str(ds4)
str(ds5)
str(ds6)
str(ds5)
str(ds6)
str(ds7)
str(ds8)
str(ds9)
str(ds10)
str(ds11)
str(ds12)

#Combining all the data frames into single data frame
all_trips <- rbind(ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9, ds10, ds11, ds12)

# inspect union table
colnames(all_trips)  #List of column names
nrow(all_trips)  #No of rows
ncol(all_trips)  # No of columns
dim(all_trips)  #Dimensions 

# to view few rows of the data
head(all_trips) 

# For numerics to get statistical summary
summary(all_trips) 

# see how many items have NA values
colSums(is.na(all_trips))

# Dropping the columns which have NA values
limit_trips <- all_trips %>%  
  select(-c(start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))

# see how many items have NA values in latest data
colSums(is.na(limit_trips))


# filter out the inconsistent data (negative time values)
limit_trips <- limit_trips %>% 
  filter(limit_trips$started_at < limit_trips$ended_at)

#Now, lets split times into separate columns using lubridate
limit_trips$date <- as.Date(limit_trips$started_at) #The default format is yyyy-mm-dd

#limit_trips$hour <- format(as.POSIXct(limit_trips$started_at,format="%H")) # not working

limit_trips$hour <- format(as.POSIXct(limit_trips$started_at,format="%H:%M:%S"),"%H")

limit_trips$day <- format(as.Date(limit_trips$started_at), "%d")
limit_trips$month <- format(as.Date(limit_trips$started_at), "%m") # to get month digit

limit_trips$month <- format(as.Date(limit_trips$started_at), "%B") # to get month as jan.feb

limit_trips$year <- format(as.Date(limit_trips$started_at), "%Y")

limit_trips$day_of_week <- format(as.Date(limit_trips$started_at), "%A")
head(limit_trips)

# arrenging days and months in order
limit_trips$day_of_week <- ordered(limit_trips$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
limit_trips$month <- ordered(limit_trips$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
head(limit_trips)

# ride_length and formatting
limit_trips$ride_length <- difftime(limit_trips$ended_at,limit_trips$started_at) # ride lenght in seconds
is.factor(limit_trips$ride_length)
head(limit_trips)
limit_trips$ride_length <- as.numeric(as.character(limit_trips$ride_length))
head(limit_trips)
is.numeric(limit_trips$ride_length)
limit_trips$ride_length_mnt <- limit_trips$ride_length/60 # ride lenght in minutes

# hour formatting from character to numeric
limit_trips$hour <- as.numeric(as.character(limit_trips$hour))

# ride_lenght check
summary(limit_trips$ride_length)

# Adding new column with caategories of times
limit_trips$ride_length_cat <- ifelse(limit_trips$ride_length_mnt>720, "more than 12 hours",
                                      ifelse(limit_trips$ride_length_mnt>180,"3-12 hours",
                                             ifelse(limit_trips$ride_length_mnt>60,"1-3 hours",
                                                    ifelse(limit_trips$ride_length_mnt>30,"30-60 minutes",
                                                           ifelse(limit_trips$ride_length_mnt>10, "10-30 minutes",
                                                                  ifelse(limit_trips$ride_length_mnt>5, "5-10 minutes",
                                                                         ifelse(limit_trips$ride_length_mnt>1,"1-5 minutes","less than 1 minutes")))))))
limit_trips$ride_length_cat <- factor(limit_trips$ride_length_cat , levels=c("less than 1 minutes", "1-5 minutes", "5-10 minutes", "10-30 minutes", "30-60 minutes", "1-3 hours", "3-12 hours", "more than 12 hours") )

# Adding new column with categories of day time
limit_trips$daytime_cat <- ifelse(limit_trips$hour>22, "night",
                                  ifelse(limit_trips$hour>18,"evening",
                                         ifelse(limit_trips$hour>13,"afternoon",
                                                ifelse(limit_trips$hour>11,"noon",
                                                       ifelse(limit_trips$hour>9, "morning",
                                                              ifelse(limit_trips$hour>5, "early morning", "night"))))))
limit_trips$daytime_cat <- factor(limit_trips$daytime_cat, levels=c("early morning", "morning", "noon", "afternoon", "evening", "night") )

#  Adding new column of categorization of weekday and weekend
limit_trips$day_cat <- ifelse(limit_trips$day_of_week == "Saturday", "weekend",
                              ifelse(limit_trips$day_of_week == "Sunday", "weekend", "weekday"))


# mean and median of dataset(ride_length)
limit_trips %>%
  summarise(number=n(),
            average_duration=mean(ride_length),
            median_duration=median(ride_length))

#mean and median of the data excluding outliers
limit_trips %>%
  filter(ride_length_mnt>1&ride_length_mnt<=720) %>%
  summarise(number=n(),
            average_duration=mean(ride_length),
            median_duration=median(ride_length))

# Analysis

# here we will divide the data with 1000 to scale the values to small number
# we should maintain consistency for all the values

# aggregation at the first level
trips_1level <-
  limit_trips %>%
  group_by(member_casual) %>%
  summarise(number=n(),
            sum=sum(ride_length_mnt),
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

#compute the position of labels, to set them at the middle of the segment
trips_1level <- trips_1level %>%
  arrange(desc(member_casual)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum) 


#pie chart of number of rides with labels
#head(trips_1level)
ggplot(trips_1level, aes(x="", y=per_num, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "white", size=4) +
  labs(title = "How type of riders differ in number of rides?", fill= "type of rider") +
  scale_fill_brewer(palette="Set1")

#pie chart of sum of ride length with labels
ggplot(trips_1level, aes(x="", y=per_sum, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y") +
  theme(legend.position="right") +
  geom_text(aes(y = ysum, label = paste(per_sum, "%")), color = "white", size=4) +
  labs(title = "How type of riders differ in summary duration of rides?", fill= "type of rider") +
  scale_fill_brewer(palette="Set1")

#saving the vizuals 
ggsave("V1-2.png")

# aggregation at the second level - by type of bike
limit_trips$rideable_type <- factor(limit_trips$rideable_type, levels=c("classic_bike", "electric_bike", "docked_bike"))

trips_rideable_type <-
  limit_trips %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

head(trips_rideable_type)

#compute the position of labels
trips_rideable_type <- trips_rideable_type %>%
  arrange(desc(rideable_type)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum)

#ratio of number of rides
ggplot(trips_rideable_type, aes(x="", y=per_num, fill=rideable_type)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "white", size=4) +
  labs(title = "How number of rides are distributed by bike types?", fill= "Bike types") +
  facet_wrap(~member_casual)
ggsave("V2-1-1.png")


#ratio of sum of ride lengths
ggplot(trips_rideable_type, aes(x="", y=per_sum, fill=rideable_type)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ysum, label = paste(per_sum, "%")), color = "white", size=4) +
  labs(title = "How sum of ride lengths are distributed by type of bikes?", fill= "type of bikes") +
  facet_wrap(~member_casual)
ggsave("V2-1-2.png")

#column chart of median of sum of ride lengths
ggplot(trips_rideable_type, aes(x = member_casual, y = median_duration, fill = rideable_type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(median_duration,1)), vjust = 1.5, colour = "white", position = position_dodge(0.9)) +
  labs(title = "How medians of ride lengths are different by type of bikes?", fill= "type of bikes",x = "type of riders", y = "median of ride length (mnt)")
ggsave("V2-1-3.png")


# aggregation L2 - by ride length category
trips_ride_length_cat <-
  limit_trips %>%
  group_by(member_casual, ride_length_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))
#compute the position of labels
head(trips_ride_length_cat)
trips_ride_length_cat <- trips_ride_length_cat %>%
  arrange(desc(trips_ride_length_cat)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum) 

#ratio of number of rides
ggplot(trips_ride_length_cat, aes(x="", y=per_num, fill=ride_length_cat)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "white", size=4) +
  labs(title = "How category of ride lenght are distributed by types of riders?", fill= "category of ride lenght") +
  scale_fill_brewer(palette="Set1") +
  facet_wrap(~member_casual)
ggsave("V2-2-1.png")


#point plot with trendline - number with sum of ride length by each day in a year (there is limit of ride length because of one extreme value)
point_trips_ridecat <- limit_trips %>%
  filter(ride_length_mnt < 1000) %>%
  group_by(member_casual, ride_length_cat, date) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt))

ggplot(point_trips_ridecat, aes(number, sum, colour = ride_length_cat)) +
  geom_point(aes(colour = ride_length_cat), size = 2) +
  facet_wrap(~member_casual) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Patterns of distributing categories of ride length by number of rides and sum of ride length for each day", x = "number of rides (ths)", y = "sum of ride length(ths mnt)", colour = "category of ride length")
ggsave("V2-2-2.png")


# aggregation at the second level - by daytime category
trips_daytime <-
  limit_trips %>%
  group_by(member_casual, daytime_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 2.3 By daytime category

#compute the position of labels
trips_daytime <- trips_daytime %>%
  arrange(desc(trips_daytime)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum)

#pie chart of ratio of number of rides
ggplot(trips_daytime, aes(x="", y=per_num, fill=daytime_cat)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "black", size=3) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "How number of rides are distributed by daytime category?", fill= "daytime category") +
  facet_wrap(~member_casual)
ggsave("V2-3-1.png")
# aggregation at the second level - by day category
trips_daycat <-
  limit_trips %>%
  group_by(member_casual, day_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))
head(trips_daycat)

### 2.4 By day category

#compute the position of labels
trips_daycat <- trips_daycat %>%
  arrange(desc(day_cat)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum)

#ratio of number of rides
ggplot(trips_daycat, aes(x="", y=per_num, fill=day_cat)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "white", size=4) +
  scale_fill_brewer(palette="Dark2") +
  labs(title = "How number of rides are distributed by day category?", fill= "day category") +
  facet_wrap(~member_casual)
ggsave("V2-4-1.png")

#column chart of median of sum of ride lengths
ggplot(trips_daycat, aes(x = member_casual, y = median_duration, fill = day_cat)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = round(median_duration,1)), vjust = 1.5, colour = "white", position = position_dodge(.9)) +
  labs(title = "How medians of ride lengths are different by day category?", fill= "day category",x = "type of riders", y = "median of ride length (mnt)")
ggsave("V2-4-2.png")


# aggregation at the second level - by hours
trips_hours <-
  limit_trips %>%
  group_by(member_casual, hour) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))
#line chart of number of rides by hour
ggplot(trips_hours,aes(x = hour, y = number, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How numbers of rides are diferent in a day by type of riders?", fill= "type of riders",x = "hours", y = "number of rides (ths)")
ggsave("V2-5-1.png")

#line chart of sum of ride length by hour
ggplot(trips_hours,aes(x = hour, y = sum, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How sum of ride length are diferent in a day by type of riders?", fill= "type of riders",x = "hours", y = "sum of ride length (ths mnt)")
ggsave("V2-5-2.png")

#line chart of median of ride length by hour
ggplot(trips_hours,aes(x = hour, y = median_duration, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How median of ride length are diferent in a day by type of riders?", fill= "type of riders",x = "hours", y = "median of ride length (ths mnt)")
ggsave("V2-5-3.png")

# aggregation at the second level - by day of week
trips_day <-
  limit_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

#number of rides by riders - line chart with points
ggplot(trips_day, aes(x = day_of_week, y = number, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How numbers of rides are distributed in a week by type of riders?", fill= "type of riders",x = "day of week", y = "number of rides (ths)")
ggsave("V2-7-1.png")

#sum of ride length by riders - line chart with points
ggplot(trips_day, aes(x = day_of_week, y = sum, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How sum of ride lenght are distributed in a week by type of riders?", fill= "type of riders",x = "day of week", y = "sum of ride lenght (ths mnt)")
ggsave("V2-7-2.png")

#median of ride length by riders - bar chart
ggplot(trips_day, aes(x = day_of_week, y = median_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette="Set1") +
  geom_text(aes(label = round(median_duration,1)), vjust = 1.5, colour = "white", position = position_dodge(.9)) +
  labs(title = "How ride median of lenghts are diferent in a week by type of riders?", fill= "type of riders",x = "day of week", y = "median lenght of ride (mnt)")
ggsave("V2-7-3.png")


# aggregation at the second level - by month
trips_month <-
  limit_trips %>%
  group_by(member_casual, month) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by type of bike + hour
trips_hour_bike <-
  limit_trips %>%
  group_by(member_casual, hour, rideable_type) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))
head(trips_hour_bike)

### 3.1 By type of bike + hour

#line plot of number of rides in a day by type of bike
ggplot(trips_hour_bike, aes(x = hour, y = number, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by types of bikes are diferent in a hour of a day?", fill= "type of riders",x = "hour", y = "percent of number of rides")

ggsave("V3-1-1.png")

#line plot of sum of ride length in a day by type of bike
ggplot(trips_hour_bike, aes(x = hour, y = sum, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How sum of ride lenght by types of bikes are diferent in a hour of a day?", fill= "type of riders",x = "hour", y = "percent of sum of ride lenght")
ggsave("V3-1-2.png")

#line plot of median of ride length in a day by type of bike
ggplot(trips_hour_bike, aes(x = hour, y = median_duration, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How median of ride lenght by types of bikes are diferent in a hour of a day?", fill= "type of riders",x = "hour", y = "median of ride lenght (mnt)")
ggsave("V3-1-3.png")

# aggregation at the third level -  by type of bike + day of a week
trips_week_bike <-
  limit_trips %>%
  group_by(member_casual, day_of_week, rideable_type) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))
head(trips_week_bike )

### 3.2 By type of bikes + day of a week

#line plot of number of rides in a week by type of bike
ggplot(trips_week_bike, aes(x = day_of_week, y = number, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by types of bikes are diferent in a day of week?", fill= "type of riders",x = "day of week", y = "percent of number of rides")
ggsave("V3-2-1.png")

#line plot of sum of ride length in a week by type of bike
ggplot(trips_week_bike, aes(x = day_of_week, y = sum, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How sum of ride lenght by types of bikes are diferent in a day of week?", fill= "type of riders",x = "day of week", y = "percent of sum of ride lenght")
ggsave("V3-2-2.png")

#line plot of median of ride length in a day by type of bike
ggplot(trips_week_bike, aes(x = day_of_week, y = median_duration, group=rideable_type, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How median of ride lenght by types of bikes are diferent in a day of week?", fill= "type of riders",x = "day of week", y = "median of ride lenght (mnt)")
ggsave("V3-2-3.png")


# aggregation at the third level -  by type of bike + month
trips_month_bike <-
  limit_trips %>%
  group_by(member_casual, month, rideable_type) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.3 By type of bike + month

#bar chart of ration of number of rides in a year by type of bike
ggplot(trips_month_bike, aes(x = month, y = per_num, fill=rideable_type)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How ration of number of rides by types of bikes are diferent in a month of a year?", fill= "type of riders",x = "month of a year", y = "percent of number of rides")
ggsave("V3-3-1.png")

#bar chart of ration of sum of ride length in a year by type of bike
ggplot(trips_month_bike, aes(x = month, y = per_sum, fill=rideable_type)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How ration of sum of ride lenght by types of bikes are diferent in a month of a year?", fill= "type of riders",x = "month of a year", y = "percent of sum of ride lenght")
ggsave("V3-3-2.png")

#bar dodge chart of median of ride length in a year by type of bike
ggplot(trips_month_bike, aes(x = month, y = median_duration, fill=rideable_type)) +
  geom_col(position = "dodge", color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How median of ride lenght by types of bikes are diferent in a month of a year?", fill= "type of riders",x = "month of a year", y = "median of ride lenght (mnt)")
ggsave("V3-3-3.png")

# aggregation at the third level -  by ride length category + hour
trips_lengthcat_hour <-
  limit_trips %>%
  group_by(member_casual, hour, ride_length_cat) %>%
  summarise(number=n()/1000) %>%
  mutate(per_num = round(number / sum(number)*100, 2))

### 3.4 By ride length category + hour

#bar plot of ration of number of rides for each hour by ride length category
ggplot(trips_lengthcat_hour, aes(x = hour, y = per_num, fill=ride_length_cat)) +
  geom_col(color="white") +
  facet_wrap(~member_casual) +
  labs(title = "How ration of number of rides by ride length category are diferent in a day?", fill= "ride length category",x = "hour", y = "percent of number of rides")
ggsave("V3-4-1.png")

#line plot with points of number of rides for each hour by ride length category
ggplot(trips_lengthcat_hour, aes(x = hour, y = number, group=ride_length_cat, color=ride_length_cat)) +
  geom_line(size=1) +
  geom_point(size=3) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by categories of ride lenght are diferent in a day?", color= "ride length category",x = "hour", y = "number of rides (ths)")
ggsave("V3-4-2.png")


# aggregation at the third level -  by ride length category + day of a week 
trips_lengthcat_day <-
  limit_trips %>%
  group_by(member_casual, day_of_week, ride_length_cat) %>%
  summarise(number=n()/1000) %>%
  mutate(per_num = round(number / sum(number)*100, 2))

### 3.5 By ride length category + day of a week

#bar plot of ration of number of rides for each day of a week by ride length category
ggplot(trips_lengthcat_day, aes(x = day_of_week, y = per_num, fill=ride_length_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How ration of number of rides by ride length category are diferent in a week?", fill= "ride length category",x = "day of a week", y = "percent of number of rides")
ggsave("V3-5-1.png")

#line plot with points of number of rides for each day of a week by ride length category
ggplot(trips_lengthcat_day, aes(x = day_of_week, y = number, group=ride_length_cat, color=ride_length_cat)) +
  geom_line(aes(linetype=ride_length_cat), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by categories of ride lenght are diferent in a week?", fill= "ride length category",x = "day of week", y = "number of rides (ths)")
ggsave("V3-5-2.png")


# aggregation at the third level -  by ride length category + month 
trips_lengthcat_month <-
  limit_trips %>%
  group_by(member_casual, month, ride_length_cat) %>%
  summarise(number=n()/1000) %>%
  mutate(per_num = round(number / sum(number)*100, 2))

### 3.6 By ride length category + month

#bar plot of ration of number of rides for each month of a year by ride length category
ggplot(trips_lengthcat_month, aes(x = month, y = per_num, fill=ride_length_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How ration of number of rides by ride length category are diferent in a year?", fill= "ride length category",x = "month", y = "percent of number of rides")
ggsave("V3-6-1.png")

#line plot with points of number of rides for each month of a year by ride length category
ggplot(trips_lengthcat_month, aes(x = month, y = number, group=ride_length_cat, color=ride_length_cat)) +
  geom_line(aes(linetype=ride_length_cat), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by categories of ride lenght are diferent in a year?", fill= "ride length category",x = "month", y = "number of rides (ths)")
ggsave("V3-6-2.png")


# aggregation at the third level -  by day category + hour
trips_hours_daycat <-
  limit_trips %>%
  group_by(member_casual, day_cat, hour) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.7 By day category + hour

#line chart of number of rides in a day
ggplot(trips_hours_daycat,aes(x = hour, y = number, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How numbers of rides are diferent in a day and by day category?", fill= "type of riders",x = "hours", y = "number of rides (ths)") +
  facet_wrap(~day_cat)
ggsave("V3-7-1.png")

#line chart of sum of ride length in a day
ggplot(trips_hours_daycat,aes(x = hour, y = sum, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How sum of ride length are diferent in a day and by day category?", fill= "type of riders",x = "hours", y = "sum of ride length (ths mnt)") +
  facet_wrap(~day_cat)
ggsave("V3-7-2.png")

#line chart of median of ride length in a day
ggplot(trips_hours_daycat,aes(x = hour, y = median_duration, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How median of ride length are diferent in a day and by day category?", fill= "type of riders",x = "hours", y = "median of ride length (ths mnt)") +
  facet_wrap(~day_cat)
ggsave("V3-7-3.png")


# aggregation at the third level -  by day category + month
trips_month_daycat <-
  limit_trips %>%
  group_by(member_casual, day_cat, month) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.8 By day category + month

#line chart of number of rides in a year by day category
ggplot(trips_month_daycat,aes(x = month, y = number, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How numbers of rides are diferent in a year and by day category?", fill= "type of riders",x = "months", y = "number of rides (ths)") +
  facet_wrap(~day_cat)
ggsave("V3-8-1.png")

#line chart of sum of ride length in a year by day category
ggplot(trips_month_daycat,aes(x = month, y = sum, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How sum of ride length are diferent in a year and by day category?", fill= "type of riders",x = "months", y = "sum of ride length (ths mnt)") +
  facet_wrap(~day_cat)
ggsave("V3-8-2.png")

#line chart of median of ride length in a year by day category
ggplot(trips_month_daycat,aes(x = month, y = median_duration, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How median of ride length are diferent in a year and by day category?", fill= "type of riders",x = "months", y = "median of ride length (mnt)") +
  facet_wrap(~day_cat)
ggsave("V3-8-3.png")


# aggregation at the third level -  by daytime category + day category
trips_daytime_daycat <-
  limit_trips %>%
  group_by(member_casual, day_cat, daytime_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.9 By daytime category + day category

#bar chart of ration of number of rides for each daytime category by day category
ggplot(trips_daytime_daycat, aes(x = day_cat, y = per_num, fill=daytime_cat)) +
  geom_col(color="white") +
  facet_wrap(~member_casual) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "How ration of number of rides by daytime category are diferent in a week?", fill= "daytime category",x = "day category", y = "percent of number of rides")
ggsave("V3-9-1.png")


# aggregation at the third level -  by daytime category + day of a week
trips_daytime_day <-
  limit_trips %>%
  group_by(member_casual, day_of_week, daytime_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.10 By daytime category + day of a week

#line chart of number of rides for each daytime category by day of a week 
ggplot(trips_daytime_day, aes(x = day_of_week, y = number, group=daytime_cat, color=daytime_cat)) +
  geom_line(aes(linetype=daytime_cat), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  scale_color_brewer(palette="Set2") +
  labs(title = "How number of rides by daytime categories are diferent by day of a week?", fill= "daytime category",x = "day of a week", y = "number of rides (ths)")
ggsave("V3-10-1.png")

#bar chart of ration of number of rides for each daytime category by day 
ggplot(trips_daytime_day, aes(x = day_of_week, y = per_num, fill=daytime_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "How ration of number of rides by daytime category are diferent by day of a week?", fill= "daytime category",x = "day of a week", y = "percent of number of rides")
ggsave("V3-10-2.png")


# aggregation at the third level -  by daytime category + month
trips_daytime_month <-
  limit_trips %>%
  group_by(member_casual, month, daytime_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

### 3.11 By daytime category + month

#line chart of number of rides for each daytime category by month 
ggplot(trips_daytime_month, aes(x = month, y = number, group=daytime_cat, color=daytime_cat)) +
  geom_line(aes(linetype=daytime_cat), size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How number of rides by daytime categories are diferent in a year?", fill= "daytime category",x = "month", y = "number of rides (ths)")
ggsave("V3-11-1.png")

#bar chart of ration of number of rides for each daytime category by month 
ggplot(trips_daytime_month, aes(x = month, y = per_num, fill=daytime_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap(~member_casual) +
  labs(title = "How ration of number of rides by daytime categories are diferent in a year?", fill= "daytime category",x = "month", y = "percent of number of rides")
ggsave("V3-11-2.png")


