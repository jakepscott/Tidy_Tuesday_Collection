# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(here)
library(tidytuesdayR)
library(glue)

tuesdata <- tidytuesdayR::tt_load('2021-09-07')


# Explore the Data --------------------------------------------------------
#This shows for each race the year, round, circuitId (join for name), race name, 
# date, time, and a URL
tuesdata$races
#This shows the driver reference, their number, their code, first and last name, birthday, and 
# nationality
tuesdata$drivers
#This shows the 
tuesdata$driver_standings %>% arrange(desc(points))

tuesdata$lap_times

data <- tuesdata$lap_times %>% 
  left_join(tuesdata$races, by = "raceId") 


data %>% group_by(year = year(date)) %>% 
  summarise(time = mean(milliseconds)) %>% 
  ungroup() %>% 
  mutate(date = glue("{year}-01-01"),
         date = as.Date(date),
         minutes = (time/1000)/60) %>% 
  ggplot(aes(date, minutes)) +
  geom_line() +
  theme_minimal()


data %>% 
  group_by(lap) %>% 
  summarise(time = mean(milliseconds))

# Usually around 61 laps. 457 races in total
data %>% 
  group_by(raceId) %>% 
  summarise(num_laps = max(lap)) %>% 
  pull(num_laps) %>% mean()

group_by(year = year(date)) %>% 
  summarise(num_laps = mean(lap)) %>% 
  ggplot(aes(as.factor(year), num_laps)) +
  geom_col()


tuesdata$drivers %>% View()
drivers_of_interest <- tuesdata$drivers %>% filter(driverId == 825 | driverId == 851)


data <- drivers_of_interest %>% 
  left_join(tuesdata$driver_standings, by = "driverId") %>% 
  left_join(tuesdata$races, by = "raceId") 

data %>% count(forename)
