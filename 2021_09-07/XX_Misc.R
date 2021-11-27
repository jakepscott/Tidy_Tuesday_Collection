library(tidyverse)
library(ggthemes)
library(ggtext)
library(here)

font <- "Roboto Condensed"
windowsFonts(`font`=windowsFont(font))
windowsFonts()

data <- tibble(x = seq(0,1,0.01)) %>% 
  mutate(y = 100*x + rnorm(101, mean = 15, sd = 10)) 
# data %>% 
#   ggplot(aes(x,y)) +
#   geom_line() +
#   geom_smooth(color = "red") +
#   scale_x_continuous(labels = scales::percent) +
#   labs(title = "**The Jake Effect**",
#        subtitle = "Thoughtfulness steadily increases as the probability **Jake** said/did it increases",
#        caption = "Source: Logic and Reason",
#        y = "Thoughtfulness",
#        x = "Likelihood Jake did/said it") +
#   theme_economist(base_family = font, base_size = 12) +
#   theme(plot.title.position = "plot",
#         plot.title = element_markdown(size = rel(1.2)),
#         plot.subtitle = element_markdown(size = rel(.9), color = "grey20", hjust = 0.5),
#         plot.caption = element_text(face = "italic", size = rel(0.8), 
#                                     color = "grey50", hjust = 0))
# 
# data %>% 
#   ggplot(aes(x,y)) +
#   geom_line() +
#   geom_smooth(color = "red") +
#   scale_x_continuous(labels = scales::percent) +
#   labs(title = "**The Jake Effect**",
#        subtitle = "Thoughtfulness steadily increases as the probability **Jake** said/did it increases",
#        caption = "Source: Logic and Reason",
#        y = "Thoughtfulness",
#        x = "Likelihood Jake did/said it") +
#   theme_economist(base_family = font, base_size = 12) +
#   theme(plot.title.position = "plot",
#         plot.title = element_markdown(size = rel(1.2)),
#         plot.subtitle = element_markdown(size = rel(.9), color = "grey20", hjust = 0.5),
#         plot.caption = element_text(face = "italic", size = rel(0.8), 
#                                     color = "grey50", hjust = 0))

data %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  geom_smooth(color = "red") +
  labs(title = "**The Jake Effect**",
       subtitle = "Thoughtfulness steadily increases as the probability Jake said/did it increases",
       caption = "Source: Logic and Reason",
       y = "Thoughtfulness",
       x = "Likelihood Jake said/did it") +
  theme_economist(base_family = font, base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = rel(1.2)),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey50", hjust = 0))

ggsave(here("2021_09/Figures/the_truth.png"), dpi = 600, units = "in", width = 8, height = 6)
