# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(glue)
library(showtext)
library(here)
library(gt)
#font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 600)

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
  geom_col(color = "#4F90FF",
           fill = "#4F90FF") +
  scale_y_continuous(labels = function(y) glue("{y}%"),
                     expand = expansion(c(0,0.01))) +
  #coord_cartesian() +
  coord_flip(ylim = c(0,8)) +
  labs(x = NULL,
       y = "Percent of Non-Stop Words in IMDB Episode Descriptions",
       title = "It seems the Doctor is often dealing with problems of time \nand friends on Earth, using Tardis",
       subtitle = "As someone who has never seen a single episode of Doctor Who, I was hoping the most common words in \nthe IMDB descriptions would give me some insight",
       caption = "Plot: @jakepscott2020 | Data: Tidytuesday, Datardis Package") +
  # theme_minimal(base_size = 12, 
  #               base_family = "roboto") +
  theme(plot.title.position = "plot", 
        plot.background = element_rect(fill = 'grey20'),
        panel.background = element_rect(fill  = 'grey20'),
        panel.grid = element_blank(), 
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = 'white',
                                  size = rel(1.35),
                                  face = "bold"),
        plot.subtitle = element_text(color = "grey70", 
                                     size = rel(.8),
                                     face = "italic"),
        plot.caption = element_text(color = "grey70", 
                                    face = "italic",
                                    size = rel(0.74)))

ggsave(here("2021-11-23/figures/top-words.png"), dpi = 600,
       height =4, width = 4.*1.62, units = "in")


# Table -------------------------------------------------------------------
top_words %>% 
  head(10) %>% 
  select(word, prop = percent) %>% 
  mutate(prop = prop/100) %>% 
  gt() %>% 
  gt::fmt_percent(prop) %>% 
  cols_label(word = "Word",
             prop = "Percent") %>% 
  data_color(columns = prop, 
             scales::col_numeric(domain = NULL,
                                 palette = c("white","pink","red")))
