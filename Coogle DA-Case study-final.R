## PROCESS
# install packages
install.packages("tidyverse")
install.packages("plotly")
install.packages("lubridate")
install.packages("ggplot2")

# call packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(readr)

# upload Divvy datasets (csv files)
X202009_divvy <- read_csv("input data/202009-divvy-tripdata.csv")
X202010_divvy <- read_csv("input data/202010-divvy-tripdata.csv")
X202011_divvy <- read_csv("input data/202011-divvy-tripdata.csv")
X202012_divvy <- read_csv("input data/202012-divvy-tripdata.csv")
X202101_divvy <- read_csv("input data/202101-divvy-tripdata.csv")
X202102_divvy <- read_csv("input data/202102-divvy-tripdata.csv")
X202103_divvy <- read_csv("input data/202103-divvy-tripdata.csv")
X202104_divvy <- read_csv("input data/202104-divvy-tripdata.csv")
X202105_divvy <- read_csv("input data/202105-divvy-tripdata.csv")
X202106_divvy <- read_csv("input data/202106-divvy-tripdata.csv")
X202107_divvy <- read_csv("input data/202107-divvy-tripdata.csv")
X202108_divvy <- read_csv("input data/202108-divvy-tripdata.csv")

# inspect the dataframes and look for incongruencies
str(X202009_divvy)
str(X202010_divvy)
str(X202011_divvy)
str(X202012_divvy)
str(X202101_divvy)
str(X202102_divvy)
str(X202101_divvy)
str(X202102_divvy)
str(X202103_divvy)
str(X202104_divvy)
str(X202105_divvy)
str(X202106_divvy)
str(X202107_divvy)
str(X202108_divvy)

# convert types
X202009_divvy <- mutate(X202009_divvy, start_station_id = as.character(start_station_id)
                        , end_station_id = as.character(end_station_id))
X202010_divvy <- mutate(X202010_divvy, start_station_id = as.character(start_station_id)
                        , end_station_id = as.character(end_station_id))
X202011_divvy <- mutate(X202011_divvy, start_station_id = as.character(start_station_id)
                        , end_station_id = as.character(end_station_id))

# inspect changed data
str(X202009_divvy)
str(X202010_divvy)
str(X202011_divvy)
str(X202012_divvy)

# union data
all_trips <- bind_rows(X202009_divvy, X202010_divvy, X202011_divvy, X202012_divvy, X202101_divvy, X202102_divvy, X202103_divvy, X202104_divvy, X202105_divvy, X202106_divvy, X202107_divvy, X202108_divvy)

# inspect union table
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# see how many items have NA values
colSums(is.na(all_trips))


# limit data frame - non-geograph data}
limit_trips <- all_trips %>%  
  select(-c(start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))

# see how many items have NA values in limit dataset
colSums(is.na(limit_trips))

# clean up data from negative time values
limit_trips <- limit_trips %>% 
  filter(limit_trips$started_at < limit_trips$ended_at)

# r new columns
limit_trips$date <- as.Date(limit_trips$started_at) #The default format is yyyy-mm-dd
limit_trips$hour <- format(as.POSIXct(limit_trips$started_at,format="%H:%M:%S"),"%H")
limit_trips$day <- format(as.Date(limit_trips$started_at), "%d")
limit_trips$month <- format(as.Date(limit_trips$started_at), "%B")
limit_trips$year <- format(as.Date(limit_trips$started_at), "%Y")
limit_trips$day_of_week <- format(as.Date(limit_trips$started_at), "%A")

# arrenge days and months
limit_trips$day_of_week <- ordered(limit_trips$day_of_week, levels=c("pondělí", "úterý", "středa", "čtvrtek", "pátek", "sobota", "neděle"))
limit_trips$month <- ordered(limit_trips$month, levels=c("leden", "únor", "březen", "duben", "květen", "červen", "červenec", "srpen", "září", "říjen", "listopad", "prosinec"))

# ride_length and formatting
limit_trips$ride_length <- difftime(limit_trips$ended_at,limit_trips$started_at) # ride lenght in seconds
is.factor(limit_trips$ride_length)
limit_trips$ride_length <- as.numeric(as.character(limit_trips$ride_length))
is.numeric(limit_trips$ride_length)
limit_trips$ride_length_mnt <- limit_trips$ride_length/60 # ride lenght in minutes

# hour formatting
limit_trips$hour <- as.numeric(as.character(limit_trips$hour))

# ride_lenght check
summary(limit_trips$ride_length)

# categorization of ride_lengt
limit_trips$ride_length_cat <- ifelse(limit_trips$ride_length_mnt>720, "more than 12 hours",
                                              ifelse(limit_trips$ride_length_mnt>180,"3-12 hours",
                                                     ifelse(limit_trips$ride_length_mnt>60,"1-3 hours",
                                                            ifelse(limit_trips$ride_length_mnt>30,"30-60 minutes",
                                                                   ifelse(limit_trips$ride_length_mnt>10, "10-30 minutes",
                                                                          ifelse(limit_trips$ride_length_mnt>5, "5-10 minutes",
                                                                                 ifelse(limit_trips$ride_length_mnt>1,"1-5 minutes","less than 1 minutes")))))))
limit_trips$ride_length_cat <- factor(limit_trips$ride_length_cat , levels=c("less than 1 minutes", "1-5 minutes", "5-10 minutes", "10-30 minutes", "30-60 minutes", "1-3 hours", "3-12 hours", "more than 12 hours") )

# categorization of daytime
limit_trips$daytime_cat <- ifelse(limit_trips$hour>22, "night",
                                              ifelse(limit_trips$hour>18,"evening",
                                                     ifelse(limit_trips$hour>13,"afternoon",
                                                            ifelse(limit_trips$hour>11,"noon",
                                                                   ifelse(limit_trips$hour>9, "morning",
                                                                          ifelse(limit_trips$hour>5, "early morning", "night"))))))
limit_trips$daytime_cat <- factor(limit_trips$daytime_cat, levels=c("early morning", "morning", "noon", "afternoon", "evening", "night") )

# categorization of weekday and weekend
limit_trips$day_cat <- ifelse(limit_trips$day_of_week == "sobota", "weekend",
                                              ifelse(limit_trips$day_of_week == "neděle", "weekend", "weekday"))


# difference between mean and median on non-extreme values and full dataset
limit_trips %>%
summarise(number=n(),
          average_duration=mean(ride_length),
          median_duration=median(ride_length))

limit_trips %>%
filter(ride_length_mnt>1&ride_length_mnt<=720) %>%
summarise(number=n(),
          average_duration=mean(ride_length),
          median_duration=median(ride_length))

# summary of ready data
summary(limit_trips)
 

## ANALYZE
# aggregation at the first level
trips_1level <-
limit_trips %>%
group_by(member_casual) %>%
summarise(number=n(),
          sum=sum(ride_length_mnt),
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))
 
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
 
# aggregation at the second level - by ride length category
trips_ride_length_cat <-
limit_trips %>%
group_by(member_casual, ride_length_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the second level - by daytime category
trips_daytime <-
limit_trips %>%
group_by(member_casual, daytime_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the second level - by day category
trips_daycat <-
limit_trips %>%
group_by(member_casual, day_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the second level - by hours
trips_hours <-
limit_trips %>%
group_by(member_casual, hour) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the second level - by day of week
trips_day <-
limit_trips %>%
group_by(member_casual, day_of_week) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

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

# aggregation at the third level -  by type of bike + day of a week
trips_week_bike <-
limit_trips %>%
group_by(member_casual, day_of_week, rideable_type) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by type of bike + month
trips_month_bike <-
limit_trips %>%
group_by(member_casual, month, rideable_type) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by ride length category + hour
trips_lengthcat_hour <-
limit_trips %>%
group_by(member_casual, hour, ride_length_cat) %>%
summarise(number=n()/1000) %>%
mutate(per_num = round(number / sum(number)*100, 2))

# aggregation at the third level -  by ride length category + day of a week 
trips_lengthcat_day <-
limit_trips %>%
group_by(member_casual, day_of_week, ride_length_cat) %>%
summarise(number=n()/1000) %>%
mutate(per_num = round(number / sum(number)*100, 2))

# aggregation at the third level -  by ride length category + month 
trips_lengthcat_month <-
limit_trips %>%
group_by(member_casual, month, ride_length_cat) %>%
summarise(number=n()/1000) %>%
mutate(per_num = round(number / sum(number)*100, 2))

# aggregation at the third level -  by day category + hour
trips_hours_daycat <-
limit_trips %>%
group_by(member_casual, day_cat, hour) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by day category + month
trips_month_daycat <-
limit_trips %>%
group_by(member_casual, day_cat, month) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by daytime category + day category
trips_daytime_daycat <-
limit_trips %>%
group_by(member_casual, day_cat, daytime_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by daytime category + day of a week
trips_daytime_day <-
limit_trips %>%
group_by(member_casual, day_of_week, daytime_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the third level -  by daytime category + month
trips_daytime_month <-
limit_trips %>%
group_by(member_casual, month, daytime_cat) %>%
summarise(number=n()/1000,
          sum=sum(ride_length_mnt)/1000,
          median_duration=median(ride_length_mnt)) %>%
mutate(per_num = round(number / sum(number)*100, 2),
       per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the 4th level - by type of bike + hour + day category
trips_bike_hour_daycat <-
limit_trips %>%
  group_by(member_casual, hour, rideable_type, day_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the 4th level - by type of bike + month + day category
trips_bike_month_daycat <-
limit_trips %>%
  group_by(member_casual, month, rideable_type, day_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the 4th level - by ride length category + hour + day category
trips_lengthcat_hour_daycat <-
limit_trips %>%
  group_by(member_casual, hour, day_cat, ride_length_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

# aggregation at the 4th level - by ride length category + month + day category
trips_lengthcat_month_daycat <-
limit_trips %>%
  group_by(member_casual, month, day_cat, ride_length_cat) %>%
  summarise(number=n()/1000,
            sum=sum(ride_length_mnt)/1000,
            median_duration=median(ride_length_mnt)) %>%
  mutate(per_num = round(number / sum(number)*100, 2),
         per_sum = round(sum / sum(sum)*100, 2))

## SHARE

### Aggregation data to tables

#aggregation of all attributes to number of rides and sum of ride length
trips_summary <- 
  limit_trips %>%
  group_by(member_casual, rideable_type, month, day_cat, day_of_week, daytime_cat, ride_length_cat) %>%
  summarise(number=n(),
            total_duration=sum(ride_length_mnt))
write.csv(trips_summary, file = 'trips_summary.csv')

# aggregation of median of ride length by datetime attributes and categories
med_month <- aggregate(limit_trips$ride_length_mnt ~ limit_trips$member_casual +
                         limit_trips$month,FUN = mean)
write.csv(med_month, file = 'med_month.csv')

med_day_cat <- aggregate(limit_trips$ride_length_mnt ~ limit_trips$member_casual +
                           limit_trips$day_cat,FUN = mean)
write.csv(med_day_cat, file = 'med_day_cat.csv')

med_day <- aggregate(limit_trips$ride_length_mnt ~ limit_trips$member_casual +
                       limit_trips$day_of_week,FUN = mean)
write.csv(med_day, file = 'med_day.csv')

med_time_cat <- aggregate(limit_trips$ride_length_mnt ~ limit_trips$member_casual +
                            limit_trips$ride_time_cat,FUN = mean)
write.csv(med_time_cat, file = 'med_time_cat.csv')

med_bike_type <- aggregate(limit_trips$ride_length_mnt ~ limit_trips$member_casual +
                             limit_trips$rideable_type,FUN = mean)
write.csv(med_bike_type, file = 'med_bike_type.csv')

### Visualizations

### 1. level

#compute the position of labels
trips_1level <- trips_1level %>%
  arrange(desc(member_casual)) %>%
  mutate(ynum = cumsum(per_num)- 0.5*per_num, ysum = cumsum(per_sum)- 0.5*per_sum)

#pie chart of number of rides with labels
ggplot(trips_1level, aes(x="", y=per_num, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ynum, label = paste(per_num, "%")), color = "white", size=4) +
  labs(title = "How type of riders differ in number of rides?", fill= "type of rider") +
  scale_fill_brewer(palette="Set1")
ggsave("V1-1.png")

#pie chart of sum of ride length with labels
ggplot(trips_1level, aes(x="", y=per_sum, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(y = ysum, label = paste(per_sum, "%")), color = "white", size=4) +
  labs(title = "How type of riders differ in summary duration of rides?", fill= "type of rider") +
  scale_fill_brewer(palette="Set1")
ggsave("V1-2.png")

### 2.1 By type of bike

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
  labs(title = "How number of rides are distributed by type of bikes?", fill= "type of bikes") +
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
  geom_text(aes(label = round(median_duration,1)), vjust = 1.5, colour = "white", position = position_dodge(.9)) +
  labs(title = "How medians of ride lengths are different by type of bikes?", fill= "type of bikes",x = "type of riders", y = "median of ride length (mnt)")
ggsave("V2-1-3.png")
 
### 2.2 By ride length category

#compute the position of labels
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

### 2.5 By hour

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

### 2.6 By month

#number of rides by riders - line chart with points
ggplot(trips_month, aes(x = month, y = number, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How numbers of rides are diferent in a year by type of riders?", fill= "type of riders",x = "month", y = "number of rides (ths)")
ggsave("V2-6-1.png", width = 9, height = 6)

#sum of ride length by riders - line chart with points
ggplot(trips_month, aes(x = month, y = sum, group=member_casual, color=member_casual)) +
  geom_line(aes(linetype=member_casual), size=1) +
  geom_point(size=3) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "How sum of ride lenghts are distributed in a year by type of riders?", fill= "type of riders",x = "month", y = "median lenght of ride (ths mnt)")
ggsave("V2-6-2.png", width = 9, height = 6)

#median of ride length by riders - bar chart
ggplot(trips_month, aes(x = month, y = median_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette="Set1") +
  geom_text(aes(label = round(median_duration,1)), vjust = 1.5, colour = "white", size = 3, position = position_dodge(.9)) +
  labs(title = "How median of ride lenghts are diferent in a year by type of riders?", fill= "type of riders",x = "month", y = "median lenght of ride (mnt)")
ggsave("V2-6-3.png", width = 9, height = 6)

### 2.7 By day of week

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

### 3.9 By daytime category + day category

#bar chart of ration of number of rides for each daytime category by day category
ggplot(trips_daytime_daycat, aes(x = day_cat, y = per_num, fill=daytime_cat)) +
  geom_col(color="white") +
  facet_wrap(~member_casual) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "How ration of number of rides by daytime category are diferent in a week?", fill= "daytime category",x = "day category", y = "percent of number of rides")
ggsave("V3-9-1.png")

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
 
### 4.1 By type of bike + hour + day category

#grid of line plots of number of rides in a day by type of bike and by day category
ggplot(trips_bike_hour_daycat, aes(x = hour, y = number, group=rideable_type, group=day_cat, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = "How number of rides by type of bike and day category are diferent in a day?", fill= "type of riders",x = "hour", y = "number of rides (ths)")
ggsave("V4-1-1.png", width = 9, height = 6)

#grid of line plots of sum of ride length in a day by type of bike and by day category
ggplot(trips_bike_hour_daycat, aes(x = hour, y = sum, group=rideable_type, group=day_cat, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = "How sum of ride length by type of bike and day category are diferent in a day?", fill= "type of riders",x = "hour", y = "sum of ride lenght (ths mnt)")
ggsave("V4-1-2.png", width = 9, height = 6)

#grid of line plots of median of ride length in a day by type of bike and by day category
ggplot(trips_bike_hour_daycat, aes(x = hour, y = median_duration, group=rideable_type, group=day_cat, color=rideable_type)) +
  geom_line(aes(linetype=rideable_type), size=1) +
  geom_point(size=3) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = "How median of ride length by type of bike and day category are diferent in a day?", fill= "type of riders",x = "hour", y = "median of ride lenght (mnt)")
ggsave("V4-1-3.png", width = 9, height = 6)
 
### 4.2 By type of bike + month + day category

#bar chart of number of rides in a year by type of bike and by day category
ggplot(trips_bike_month_daycat, aes(x = month, y = number, group=rideable_type, group=day_cat, fill=rideable_type)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = "How number of rides by types of bikes and day category are diferent in a year?", fill= "type of riders",x = "month", y = "number of rides (ths)")
ggsave("V4-2-1.png", width = 9, height = 6)

#bar chart of sum of ride length in a year by type of bike and by day category
ggplot(trips_bike_month_daycat, aes(x = month, y = sum, group=rideable_type, group=day_cat, fill=rideable_type)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "How sum of ride length by type of bike and day category are diferent in a year?", fill= "type of riders",x = "month", y = "sum of ride lenght (ths mnt)")
ggsave("V4-2-2.png", width = 9, height = 6)

#bar chart of median of ride length in a year by type of bike and by day category
ggplot(trips_bike_month_daycat, aes(x = month, y = median_duration, group=rideable_type, group=day_cat, fill=rideable_type)) +
  geom_col(position = "dodge", color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "How median of ride length by type of bike and day category are diferent in a year?", fill= "type of riders",x = "month", y = "median of ride lenght (mnt)")
ggsave("V4-2-3.png", width = 9, height = 6)
 
### 4.3 By ride length category + hour + day category

#grid of line plots of number of rides in a day by type of bike
ggplot(trips_lengthcat_hour_daycat, aes(x = hour, y = number, group=ride_length_cat, group=day_cat, color=ride_length_cat)) +
  geom_line(size=1) +
  geom_point(size=3) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = str_wrap("How number of rides by ride length category and day category are diferent in a day?",65), fill= "type of riders",x = "hour", y = "number of rides (ths)")
ggsave("V4-3-1.png", width = 9, height = 6)

#bar chart of ration of number of rides in a day by ride length category
ggplot(trips_lengthcat_hour_daycat, aes(x = hour, y = per_num, group=ride_length_cat, group=day_cat, fill=ride_length_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = str_wrap("How ration of number of rides by ride length category and day category are diferent in a day?",65), fill= "ride length category",x = "hour", y = "ration of number of ride lenght")
ggsave("V4-3-2.png", width = 9, height = 6)

### 4.4 By ride length category + month + day category

#grid of line plots of number of rides in a day by ride length category
ggplot(trips_lengthcat_month_daycat, aes(x = month, y = number, group=ride_length_cat, group=day_cat, color=ride_length_cat)) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  labs(title = str_wrap("How number of rides by ride length category and day category are diferent in a year?",65), color= "ride length category",x = "month", y = "number of rides (ths)")
ggsave("V4-4-1.png", width = 9, height = 6)

#bar chart of ration of number of rides in a year by ride length category
ggplot(trips_lengthcat_month_daycat, aes(x = month, y = per_num, group=ride_length_cat, group=day_cat, fill=ride_length_cat)) +
  geom_col(color="white") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(day_cat ~ member_casual, margins = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = str_wrap("How ration of number of rides by ride length category and day category are diferent in a year?", 65), fill= "ride length category",x = "month", y = "ration of number of ride lenght")
ggsave("V4-4-2.png", width = 9, height = 6)