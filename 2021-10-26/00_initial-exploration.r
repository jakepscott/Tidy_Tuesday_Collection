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
#Just the runners
runners <- ultra_rankings %>% 
  distinct(runner, gender, nationality) %>% 
  mutate(gender = case_when(gender == "M" ~ "Male",
                            gender == "W" ~ "Female",
                            TRUE ~ NA_character_))

#Just the top runners
top_runners <- ultra_rankings %>% 
  filter(rank == 1) %>% 
  count(runner, sort = T) %>% 
  head(15)

# Runners times over time
runner_times <- top_runners_with_races %>% 
  arrange(runner,date) %>% 
  select(date, rank, runner, time) %>% 
  mutate(year_mon = zoo::as.yearmon(date)) %>% 
  na.omit() %>% 
  group_by(runner, year=lubridate::year(date)) %>% 
  summarise(time = mean(time)) %>% 
  ungroup() %>% 
  mutate(time = as.numeric(time),
         time = time/60/60) %>% 
  group_by(runner) %>% 
  summarise(time = list(time))

# Distribution of rank
position_finishes <- top_runners_with_races %>% 
  arrange(runner) %>% 
  select(runner, rank) %>% 
  mutate(position = case_when(
    rank == 1 ~ "First",
    rank > 1 & rank <=5 ~ "In Top 5",
    TRUE ~ "Outside Top 5"
  )) %>%
  count(runner, position) %>%
  mutate(position = fct_relevel(position, "First", "In Top 5", "Outside Top 5")) %>% 
  group_by(runner) %>% 
  summarise(rank = list(n)) 


position_finishes_percent <- top_runners_with_races %>% 
  arrange(runner) %>% 
  select(runner, rank) %>% 
  mutate(position = case_when(
    rank == 1 ~ "First",
    rank > 1 & rank <=5 ~ "In Top 5",
    TRUE ~ "Outside Top 5"
  )) %>%
  mutate(position = fct_relevel(position, "First", "In Top 5", "Outside Top 5")) %>% 
  count(runner, position) %>%
  group_by(runner) %>% 
  mutate(percent = round(n / sum(n)*100, digits = 0)) %>% 
  summarise(rank = list(percent)) 

# RUnners with best ranks among those with 5 wins
five_wins <- top_runners %>% 
  filter(n == 5)

top4_of_5_wins <- ultra_rankings %>% 
  filter(runner %in% five_wins$runner) %>% 
  group_by(runner) %>% 
  summarise(rank = mean(rank, na.rm = T)) %>% 
  arrange(rank) %>% 
  head(4)

# Initial Tables ----------------------------------------------------------
initial_table <- ultra_rankings %>% 
  filter(rank == 1) %>% 
  count(runner, sort = T) %>% 
  filter(n > 5 | (runner %in% top4_of_5_wins$runner)) %>% 
  left_join(runners) %>%
  left_join(runner_times) %>%
  left_join(position_finishes_percent) %>% 
  select(-gender) %>% 
  rename(Runner = runner,
         Wins = n,
         Nationality = nationality) %>% 
  mutate(Nationality_flag = case_when(
    Nationality == "USA" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/united_states_of_america_round_icon_640.png",
    Nationality == "FRA" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/france_round_icon_640.png",
    Nationality == "GBR" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/united_kingdom_round_icon_640.png",
    Nationality == "ESP" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/spain_round_icon_640.png",
    Nationality == "CHN" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/china_round_icon_640.png",
    Nationality == "GER" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/germany_round_icon_640.png",
    Nationality == "AUT" ~ "https://raw.githubusercontent.com/jakepscott/Tidy_Tuesday_Collection/master/2021-10-26/imgs/austria_round_icon_640.png"
    ),
  ) %>% 
  relocate(Nationality_flag, .before = Wins) %>% 
  gt() %>% 
  cols_label(
    Nationality_flag = "Nationality",
    time = "Times Over Career",
    Wins = "First place finishes"
  ) 

initial_table

# Add extras ---------------------------------------------------------------
initial_table <- initial_table %>% 
  gtExtras::gt_theme_nytimes() %>% 
  gtExtras::gt_merge_stack(col1 = Runner, col2 = Nationality) %>% 
  gtExtras::gt_fa_repeats(
    column = Wins, 
    palette = "goldenrod",
    name = "trophy",
    align = "left"
  ) %>% 
  gtExtras::gt_img_rows(columns = Nationality_flag) %>% 
  cols_align(align = "center",
             columns = Nationality_flag) %>% 
  gtExtras::gt_sparkline(
    # Select column with data
    time,
    # Color for min/max points
    range_colors=c("blue","red"),
    # Line color
    line_color="#DBDFE6",
    # Hide labels 
    # (for latest versions of {gtExtras}) only:
    label=FALSE
  ) 


#Add bar plot
initial_table %>% 
  gt_plt_bar_stack(
    # Column with data
    column=rank,
    # Stacked barplot 
    position = 'stack', 
    # # Set labels and color
    labels = c("First Place", "Top 5", "Outside Top 5"),
    palette = c("goldenrod", "grey50", "brown"),
    # # Barplot width
    width = 60,
    # # Same size for all labels
    trim=TRUE
  ) %>% 
  tab_spanner(
    label = md("**Percent** of races runner finished in given group"),
    columns = c(rank)
    ) %>% 
  tab_header(title = "Ultra Trail Running First Place Finishes",
             subtitle = md("\"Ultra Trail Runs\", are runs that are **42 kilometers** or longer and that take place on an **unpaved** surface. These runs often, though not always, include huge elevation changes; the *Gede Pangrango 100* race in 2018 had an elevation gain of **14430** meters. They draw runners from around the world, with winners spanning almost 60 countries (though the USA and Great Britain hold a disproportionte share of first place finishes)")
             )

