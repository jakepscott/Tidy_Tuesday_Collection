
# Load Libraries ----------------------------------------------------------
library(tidytext)
library(tidyverse)
library(rvest)
#Make sure roboto condensed is downloaded onto your machine
font <- "Roboto Condensed"
windowsFonts(`font`=windowsFont(font))
windowsFonts()

# Load Data ---------------------------------------------------------------
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')


# Setting Plot Theme ------------------------------------------------------
jake_theme <- theme_minimal(base_family = "font", base_size = 12) +
  theme(plot.title = element_text(face="bold", size = rel(1), color="white"),
        plot.subtitle = element_text(size=rel(.8),colour = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.title = element_blank(),
        axis.text.x  = element_text(color="white", size=rel(.7)),
        axis.text.y  = element_text(color="white", size=rel(.7)),
        strip.text = element_text(face = "bold", color = "white", size=rel(.7)),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='grey10',color=NA))


# Taylor Albums tf-idf -----------------------------------------------------------
#Getting the count of each word by album
(Tay_Lyrics <- taylor_swift_lyrics %>% 
   select(Album, Lyrics) %>%
   unnest_tokens(word,Lyrics) %>% 
   count(Album, word, sort = TRUE))

#Capitalizing Album Names
Tay_Lyrics <- Tay_Lyrics %>% mutate(Album=str_replace(Album,"reputation","Repuation"),
                                    Album=str_replace(Album,"folklore","Folklore"))

#Getting tf_idf value
Tay_Lyrics <- Tay_Lyrics %>% 
  bind_tf_idf(word, Album, n) %>% 
  arrange(desc(tf_idf))

#Ordering albums in order of release date
Tay_Lyrics$Album <- factor(Tay_Lyrics$Album,
                           levels=c("Taylor Swift", "Fearless","Speak Now",
                                    "Red", "1989", "Reputation", "Lover", "Folklore"),
                           labels = c("Taylor Swift", "Fearless","Speak Now",
                                      "Red", "1989", "Reputation", "Lover", "Folklore"))
#Ridding of Albums not captured in Album column
Tay_Lyrics <- Tay_Lyrics %>% filter(!is.na(Album))

#Graphing
Tay_Lyrics %>%
  mutate(word=str_to_title(word)) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% #Not sure what is happening here
  group_by(Album) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, color = Album)) +
  geom_pointrange(aes(ymin = 0, ymax = tf_idf),
                  show.legend = FALSE) +
  facet_wrap(~Album, ncol = 2, scales = "free") +
  coord_flip() +
  scale_y_continuous(expand = c(.02,0)) +
  labs(title="Most Important Words for Each Taylor Swift Album",
       subtitle = "Relative importance measured using tf-idf methodology",
       caption = "Plot: @jakepscott2020 | Data: Rosie Baillie & Dr. Sara Stoudt") +
  jake_theme

ggsave("9_29_2020/Tay_tf_idf_plot.png",dpi=600)


# Beyonce Albums tf-idf -----------------------------------------------------------
# Manually assign songs to albums(from KellyCotton https://github.com/kellycotton/TidyTuesdays/blob/master/code/beyonce.R)
DangerLove <- c("Crazy in Love (Ft. JAY-Z)", "Naughty Girl", "That's How You Like It (Ft. JAY-Z)", "Baby Boy (Ft. Sean Paul)", "Hip Hop Star (Ft. Big Boi & Sleepy Brown)", "Be With You", "Me, Myself, and I", "Yes", "Signs (Ft. Missy Elliott)", "Speechless", "The Closer I Get to You (Ft. Luther Vandross)", "Dangerously in Love 2", "Beyoncé Interlude", "Gift From Virgo", "Daddy")
Bday <- c("Déjà Vu (Ft. JAY-Z)", "Get Me Bodied", "Suga Mama", "Upgrade U (Ft. JAY-Z)", "Ring the Alarm", "Kitty Kat", "Freakum Dress", "Green Light", "Irreplaceable", "Resentment", "Check On It (Ft. Bun B & Slim Thug)", "Encore for the Fans", "Listen" )
Sasha <- c("If I Were a Boy", "Halo", "Disappear", "Broken-hearted Girl", "Ave Maria", "Smash Into You", "Satellites", "That's Why You're Beautiful", "Save the Hero", "Single Ladies (Put a Ring on It)", "Radio", "Diva", "Sweet Dreams", "Video Phone", "Hello", "Ego")
Four <- c("1+1", "I Care", "I Miss You", "Best Thing I Never Had", "Party (Ft. André 3000)", "Rather Die Young", "Start Over", "Love on Top", "Countdown", "End of Time", "I Was Here", "Run the World (Girls)", "Lay Up Under Me", "Schoolin' Life", "Dance for You")
Beyonce <- c("Pretty Hurts", "Haunted", "Drunk in Love (Ft. JAY-Z)", "Blow", "No Angel", "Partition", "Jealous", "Rocket", "Mine (Ft. Drake)", "XO", "***Flawless (Ft. Chimamanda Ngozi Adichie)", "Superpower (Ft. Frank Ocean)", "Heaven", "Blue (Ft. Blue Ivy Carter)")
Lemonade <- c("Pray You Catch Me", "Hold Up", "Don't Hurt Yourself", "Sorry",
              "6 Inch", "Daddy Lessons","Love Drought","Sandcastles", "Forward",
              "Freedom", "All Night","Formation")

#Classifying Songs by album
Bey_Lyrics <- beyonce_lyrics %>% 
  filter(song_name %in% c(DangerLove, Bday, Sasha, Four, Beyonce,Lemonade)) %>% 
  mutate(Album = case_when(
    song_name %in% DangerLove ~ "Dangerously in Love",
    song_name %in% Bday ~ "B'Day",
    song_name %in% Sasha ~ "I Am... Sasha Fierce",
    song_name %in% Four ~ "4",
    song_name %in% Beyonce ~ "Beyoncé",
    song_name %in% Lemonade ~ "Lemonade"
  )) %>% rename("Lyrics"=line)

#Need to order albums in order of release
Bey_Lyrics$Album <- factor(Bey_Lyrics$Album,
                           levels=c("Dangerously in Love", "B'Day","I Am... Sasha Fierce",
                                    "4", "Beyoncé", "Lemonade"),
                           labels = c("Dangerously in Love", "B'Day","I Am... Sasha Fierce",
                                      "4", "Beyoncé", "Lemonade"))

#Getting the count of each word by album
(Bey_Lyrics <- Bey_Lyrics %>% 
   select(Album, Lyrics) %>%
   unnest_tokens(word,Lyrics) %>% 
   count(Album, word, sort = TRUE))

#Getting tf_idf value
Bey_Lyrics <- Bey_Lyrics %>% 
  bind_tf_idf(word, Album, n) %>% 
  arrange(desc(tf_idf))

#Graphing
Bey_Lyrics %>%
  mutate(word=str_to_title(word)) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% #Not sure what is happening here
  group_by(Album) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, color = Album)) +
  geom_pointrange(aes(ymin = 0, ymax = tf_idf),
                  show.legend = FALSE,
                  size=1.1,
                  fatten=3) +
  facet_wrap(~Album, ncol = 2, scales = "free") +
  coord_flip() +
  scale_y_continuous(expand = c(.03,0)) +
  labs(title="Most Important Words for Each Beyoncé Album",
       subtitle = "Relative importance measured using tf-idf methodology",
       caption = "Plot: @jakepscott2020 | Data: Rosie Baillie & Dr. Sara Stoudt") +
  jake_theme 

ggsave("9_29_2020/Bey_tf_idf_plot.png",dpi=600)
