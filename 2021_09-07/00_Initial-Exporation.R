# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(here)
library(tidytuesdayR)
library(patchwork)

font <- "Roboto Condensed"
windowsFonts(`font`=windowsFont(font))
windowsFonts()

driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')


# Collecting Data ---------------------------------------------------------
# driver_standings %>% arrange(desc(wins))
# races %>% arrange(raceId) %>% View()
# drivers 
# lap_times

data <- lap_times %>% 
  left_join(races, by = "raceId") %>% 
  left_join(drivers, by = "driverId") 

data <- data %>% 
  mutate(mob = month(dob, label = T), 
         minutes = milliseconds/1000/60)

theme_set(theme_minimal(base_family = "font", base_size = 12))

(bad <- data %>% 
  group_by(mob) %>% 
  summarise(avg_lap_time = mean(minutes),
            sd = sd(minutes)) %>% 
  ungroup() %>% 
  #mutate(label = emoji("racing_car")) %>% 
  ggplot(aes(mob, 
             #label=label,
             avg_lap_time)) +
  geom_col(color = "lightblue", 
           fill = "lightblue") +
  #geom_text(family="EmojiOne") +
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
                                    color = "grey50")))

ggsave(here("2021_09/Figures/misleading_plot.png"), dpi = 600, bg = "white")


(better <- data %>% 
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
                                    color = "grey50")))

ggsave(here("2021_09/Figures/better_plot.png"), dpi = 600, bg = "white")


#Join the two plots

bad + better + plot_layout(ncol = 1)
ggsave(here("2021_09/Figures/joined_plot.png"), dpi = 600, bg = "white")
