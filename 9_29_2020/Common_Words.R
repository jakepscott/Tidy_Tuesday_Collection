
# Load Libraries ----------------------------------------------------------
library(tidytext)
library(tidyverse)
library(ggrepel)
library(here)
library(ggtext)

#Make sure roboto condensed is downloaded onto your machine
font <- "Roboto Condensed"
windowsFonts(`font`=windowsFont(font))
windowsFonts()

# Load Data ---------------------------------------------------------------
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')


bey <- beyonce_lyrics %>% 
  select('Artist'=artist_name,"Title"=song_name,"Lyrics"=line) 
tay <- taylor_swift_lyrics %>% select(Artist,Lyrics,Title)

data <- tay %>% bind_rows(bey)

data <- data %>% unnest_tokens(word,Lyrics)

data <- data %>% 
  filter(!(word %in% c(get_stopwords()$word,"oh","like"))) %>% 
  count(Artist,word,sort = T) %>% 
  group_by(Artist) %>% 
  mutate(total=sum(n),
         percent=n/total*100) %>% 
  ungroup() %>% 
  group_by(word) %>% 
  filter(percent>.6) %>% 
  top_n(wt=percent,20) %>% 
  ungroup() %>% 
  select(-n,-total) %>% 
  pivot_wider(names_from = Artist,values_from=percent,values_fill = 0)

data %>% 
  ggplot(aes(`Beyoncé`,`Taylor Swift`)) +
  geom_abline(color="grey70",lty=2,alpha=.75,lwd=2) +
  geom_text_repel(aes(label=word)) +
  #Point to Taylor Words
  geom_curve(aes(x = .3, y = 1.5, xend = .15, yend = 1),
             curvature = -.5, #A numeric value giving the amount of curvature. Negative values produce left-hand curves, positive values produce right-hand curves, and zero produces a straight line.
             angle = 110, #A numeric value between 0 and 180, giving an amount to skew the control points of the curve. Values less than 90 skew the curve towards the start point and values greater than 90 skew the curve towards the end point.
             color = "red",
             size = 1,
             arrow = arrow(length = unit(0.025, "npc"),
                           type = "closed")) +
  #Annotate Taylor Words
  geom_richtext(y=1.6,x=.3,
                label="*Words like \"never\" and \"back\" are <br/>said more often by Taylor Swift*",
                label.color = 'red',
                size=4) +
  #Point to Beyonce Words
  geom_curve(aes(x = 1, y = .4, xend = .75, yend = .15),
             curvature = .5, #A numeric value giving the amount of curvature. Negative values produce left-hand curves, positive values produce right-hand curves, and zero produces a straight line.
             angle = 110, #A numeric value between 0 and 180, giving an amount to skew the control points of the curve. Values less than 90 skew the curve towards the start point and values greater than 90 skew the curve towards the end point.
             color = "blue",
             size = 1,
             arrow = arrow(length = unit(0.025, "npc"),
                           type = "closed")) +
  #Annotate Beyonce Words
  geom_richtext(y=.4,x=1.3,
                label="*Words like \"ain't\" and \"girl\" are <br/>said more often by Beyoncé*",
                label.color = "blue",
                size=4) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + # Add percent sign 
  labs(title="Which lyrics are sung more often by Beyoncé vs Taylor Swift?",
       subtitle = "The axes represent the percent of total lyrics a given word, like \"love,\" makes up",
       caption = "Plot: @jakepscott2020 | Data: Rosie Baillie & Dr. Sara Stoudt") +
  theme_minimal(base_family = font, base_size = 12) +
  theme(plot.title =element_text(size=rel(1.6),face="bold"),
        plot.subtitle = element_markdown(size = rel(1.25),face="italic"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        legend.position = "none"
  )

ggsave(here("9_29_2020/Relative_Words.png"),dpi=600, width = 8, height = 6)

