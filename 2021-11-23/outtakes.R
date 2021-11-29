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
             scales::col_numeric(domain = c(0,.1),
                                 palette = c("white","pink","red")))
