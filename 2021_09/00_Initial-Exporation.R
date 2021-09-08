# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(here)
library(tidytuesdayR)
library(glue)

font <- "Roboto Condensed"
windowsFonts(`font`=windowsFont(font))
windowsFonts()

tuesdata <- tidytuesdayR::tt_load('2021-09-07')


# Collecting Data ---------------------------------------------------------
tuesdata$driver_standings %>% arrange(desc(wins))
tuesdata$races %>% arrange(raceId) %>% View()
tuesdata$drivers 
tuesdata$lap_times

data <- tuesdata$lap_times %>% 
  left_join(tuesdata$races, by = "raceId") %>% 
  left_join(tuesdata$drivers, by = "driverId") 

data <- data %>% 
  mutate(mob = month(dob, label = T), 
         minutes = milliseconds/1000/60)

theme_set(theme_minimal(base_family = "font", base_size = 12))

data %>% 
  group_by(mob) %>% 
  summarise(avg_lap_time = mean(minutes),
            sd = sd(minutes)) %>% 
  ungroup() %>% 
  ggplot(aes(mob, avg_lap_time)) +
  geom_col(color = "lightblue", 
           fill = "lightblue") +
  labs(title = "Drivers born in May *conclusively* drive the fastest laps, followed by November babies",
       subtitle = "Must be something about odd numbered months...",
       caption = "Plot: @jakepscott2020 | Data: Ergast, TidyTuesday",
       x = "Month of Birth",
       y = "Average Lap Time (Min)") +
  coord_cartesian(ylim = c(1.525, NA)) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(face="bold", size = rel(1), color="black"),
        plot.subtitle = element_text(size=rel(.8),colour = "grey20"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey50"))

ggsave(here("2021_09/Figures/misleading_plot.png"), dpi = 600, bg = "white")


data %>% 
  group_by(mob) %>% 
  summarise(avg_lap_time = mean(minutes),
            sd = sd(minutes)) %>% 
  ungroup() %>% 
  ggplot(aes(mob, avg_lap_time)) +
  geom_errorbar(aes(ymin = avg_lap_time - sd, ymax = avg_lap_time + sd),
                color = "grey20", 
                width = .2) +
  geom_point(color = "lightblue",
             fill = "lightblue",
             size = 5) +
  labs(title = "Or maybe there isn't any real difference!",
       subtitle = "There is hope for us April babies yet...",
       caption = "Plot: @jakepscott2020 | Data: Ergast, TidyTuesday",
       x = "Month of Birth",
       y = "Average Lap Time (Min)") +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(face="bold", size = rel(1), color="black"),
        plot.subtitle = element_text(size=rel(.8),colour = "grey20"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey50"))