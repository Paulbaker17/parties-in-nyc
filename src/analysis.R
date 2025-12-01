library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(lubridate)

## TODO LIST

# Where is the party
# What location type is the party at
# Combine both of above in one graph
# Create a line graph or radial over a 24 hour period
# Create a bar graph over a week
# Create a bar graph over a year by month
# How long was the response time
# Is the response time on average different per borough


data <- read.csv("data//party_in_nyc.csv")

head(data)

dim(data)

glimpse(data)

summary(data)

table(data$Borough)

#Borough is sometimes 


table(data$City)

#City seems to be neighbourhood, with some just having the borough instead

#Currently borough data is somewhat useful but City data is not

#Lets get a better idea of distribution by plotting them on a map, need to improve on this

usa <- map_data("usa")

ggplot() +
  #geom_polygon(data = usa, aes(x = long, y = lat, group = group),
  #             fill = "lightgray", color = "white") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), size = 0.1) +
  #coord_cartesian(xlim = c(-74.3, -73.3), ylim = c(40.5, 41)) +  # zoom to NY area
  labs(title = "Points Around New York",
       x = "Longitude", y = "Latitude")


ggplot(data = data, mapping = aes(x = Borough)) +
  geom_bar()

ggplot(data = data, mapping = aes(x = Location.Type)) +
  geom_bar()

ggplot(data = data, mapping = aes(x = Borough)) +
  geom_bar(mapping = aes(fill = Location.Type), position = "dodge")

data%>%
  mutate(Hour = hour(data$Created.Date)) %>%
  ggplot(mapping = aes(x = Hour)) +
  geom_bar() +
  ylim(-20000,35000) + 
  coord_polar(start = 0)

data%>%
  mutate(Day = wday(data$Created.Date, , label = TRUE, week_start = 1)) %>%
  ggplot(mapping = aes(x = Day)) +
  geom_bar()

data%>%
  mutate(Month = month(data$Created.Date, label = TRUE)) %>%
  ggplot(mapping = aes(x = Month)) +
  geom_bar()

data%>%
  mutate(Month = month(data$Created.Date, label = TRUE)) %>%
  ggplot(mapping = aes(x = Month)) +
  geom_bar()

data%>%
  mutate(Response.Time = (interval(Created.Date, Closed.Date) %/% minutes(1))) %>%
  filter(as.Date("2015-01-01") <= Created.Date & as.Date("2015-01-01") <= Closed.Date) %>%
  filter(Borough != "Unspecified") %>%
  ggplot(aes(y = Response.Time, x = Borough)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim =  c(0, 1200))

         