# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(glue)
library(showtext)
library(here)
#font_add_google("Roboto", "roboto")
showtext_auto()
# Load Data ---------------------------------------------------------------
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')
#write_csv(imdb,here("2021-11-23/data/imbd.csv"))


# Clean Data --------------------------------------------------------------
top_words <- imdb %>% 
  select(desc) %>% 
  unnest_tokens(output = "word", input = desc, token = "words") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = n/total*100, 
         word = str_to_title(word))


# Plot --------------------------------------------------------------------
top_words %>% 
  head(10) %>% 
  ggplot(aes(fct_reorder(word, percent), percent)) +
  geom_col(color = "#3D24FF",
           fill = "#3D24FF") +
  scale_y_continuous(labels = function(y) glue("{y}%")) +
  coord_flip() +
  labs(x = NULL,
       y = "Percent of Non-Stop Words in IMDB Episode Descriptions",
       title = "It seems the Doctor is often dealing with problems of time and friends on Earth, using Tardis",
       subtitle = "As someone who has never seen a single episode, I was hoping the most common words in the IMDB descriptions would give me some insight",
       caption = "Plot: @jakepscott2020 | Data: Tidytuesday, Datardis Package") +
  theme_minimal(base_size = 12, 
                base_family = "roboto") +
  theme(plot.title.position = "plot", 
        plot.background = element_rect(fill = 'grey20'),
        panel.background = element_rect(fill  = 'grey20'),
        panel.grid = element_blank(), 
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = 'white'),
        plot.subtitle = element_text(color = "grey70"),
        plot.caption = element_text(color = "grey70", face = "italic"))
