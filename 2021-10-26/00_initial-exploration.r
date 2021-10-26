# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(gt)
library(gtExtras)
library(here)

# Load Data ----------------------------------------------------------------
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


# Clean data --------------------------------------------------------------
ultra_rankings <- ultra_rankings %>% 
  mutate(runner = str_to_title(runner))  
  
# Create support tables ---------------------------------------------------
runners <- ultra_rankings %>% 
  distinct(runner, gender, nationality) %>% 
  mutate(gender = case_when(gender == "M" ~ "Male",
                            gender == "W" ~ "Female",
                            TRUE ~ NA_character_))


# Top runners -------------------------------------------------------------
top_runners <- ultra_rankings %>% 
  filter(rank == 1) %>% 
  count(runner, sort = T) %>% 
  head(15)

# Initial Tables ----------------------------------------------------------
ultra_rankings %>% 
  filter(rank == 1) %>% 
  count(runner, sort = T) %>% 
  head(15) %>%
  left_join(runners) %>% 
  rename(Runner = runner,
         Wins = n,
         Sex = gender,
         Nationality = nationality) %>% 
  mutate(case_when(
    Nationality == "USA" ~ here("2021-10-26/imgs/united_states_of_america_round_icon_640.png"),
    Nationality == "FRA" ~ here("2021-10-26/imgs/france_round_icon_640.png"),
    Nationality == "GBR" ~ here("2021-10-26/imgs/united_kingdom_round_icon_640.png"),
    Nationality == "ESP" ~ here("2021-10-26/imgs/spain_round_icon_640.png"),
    Nationality == "CHN" ~ here("2021-10-26/imgs/china_round_icon_640.png"),
    Nationality == "GER" ~ here("2021-10-26/imgs/germany_round_icon_640.png"),
    Nationality == "AUT" ~ here("2021-10-26/imgs/austria_round_icon_640.png"),
  ))
  gt() %>% 
  gtExtras::gt_theme_nytimes() %>% 
  gtExtras::gt_merge_stack(col1 = Runner, col2 = Sex) %>% 
  gtExtras::gt_fa_repeats(
    column = Wins, 
    palette = "gold",
    name = "trophy",
    align = "left"
  )
  gtExtras::gt_fa_repeats(
    column=Titles,
    palette = "orange",
    name = "tshirt",
    align='left'
  )
