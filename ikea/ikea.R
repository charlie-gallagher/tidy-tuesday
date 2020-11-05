library(tidyverse)
library(ggrepel)
library(extrafont)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv', col_types = cols(
  .default = col_character(),
  X1 = col_double(),
  price = col_double(),
  old_price = col_character(),
  sellable_online = col_logical(),
  depth = col_double(),
  height = col_double(),
  width = col_double()
)) %>% 
  select(-X1) %>% 
  mutate(
    old_price = case_when(
      old_price == "No old price" ~ NA_character_,
      TRUE ~ old_price
    ),
    old_price = str_replace_all(old_price, "(SR )|(,)", ""),
    old_price = as.numeric(old_price),
    other_colors = case_when(
      other_colors == "No" ~ FALSE,
      other_colors == "Yes" ~ TRUE
    )
  )



ikea_names <- ikea %>%
  select(name) %>% 
  unique() %>% 
  mutate(
    cons = str_count(tolower(name), "[bcdfghjklmnpqrstvwxz]"),
    vow = str_count(tolower(name), "[aeiouyöäå]"),
    vow_spec = str_count(tolower(name), "[öäå]"),
    nchar = nchar(name),
    ratio = vow / cons,
    pct_cons = cons / nchar
  ) %>% 
  select(name, cons:pct_cons)



# So far, the results have not been great. I think it would help greatly if
# the data were more naturally spread out, and also if some were dark while 
# others were light, and the light ones put on top. 
# Using ggrepel

# New idea: Evenly distribute results with ranks and alphabetization. See below.

p1 <- ikea_names %>%
  mutate(
    txt_color = round(runif(n = 607, min = 1, max = 2)),
    size = rchisq(n = 607, df = 10),
    first_letter = substr(name, 1, 1)
  ) %>%
  arrange(color) %>% 
  ggplot() + 
  geom_text_repel(aes(x = first_letter, y = ratio, label = name,
                color = factor(color), size = size),
             position = position_jitter(width = 2, height = 2),
             segment.alpha = 0, seed = 321) + 
  labs(
    title = "IKEA"
  ) + 
  scale_y_reverse() + 
  scale_size_continuous(range(2, 6)) + 
  scale_color_manual(values = c('#4b5c73ff', '#ffffff')) +
  guides(size = FALSE, color = FALSE) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#2c3643ff", color = NA),
    plot.title = element_text(
      family = "Roboto Bk", 
      size = 60, 
      color = 'white'),
    plot.margin = margin(20, 10, 20, 10),
    panel.background = element_rect(fill = "#2c3643ff", color = NA)
  )

svg(filename = "ikea.svg", width = 14, height = 11)
p1
dev.off()





# Even distribution of words --------------------
ikea_names <- ikea %>%
  select(name) %>% 
  unique() %>% 
  mutate(
    first_letter = substr(name, 1, 1),
    cons = str_count(tolower(name), "[bcdfghjklmnpqrstvwxz]"),
    vow = str_count(tolower(name), "[aeiouyöäå]"),
    vow_spec = str_count(tolower(name), "[öäå]"),
    nchar = nchar(name),
    ratio = vow / cons,
    pct_cons = cons / nchar
  ) %>% 
  select(name, first_letter:pct_cons) %>% 
  group_by(first_letter) %>% 
  mutate(
    rank = row_number(desc(ratio))
  ) %>% 
  arrange(first_letter, rank)


p2 <- ikea_names %>% 
  ungroup() %>% 
  mutate(
    txt_color = round(runif(n = 607, min = 1, max = 2)),
    txt_size = rchisq(n = 607, df = 10)
  ) %>% 
  arrange(txt_color) %>% 
  ggplot() + 
  geom_text_repel(aes(x = first_letter, y = rank, label = name,
                color = factor(txt_color), size = txt_size),
             position = position_jitter(width = 1, height = 2),
             segment.color = NA,
             force = 2) +
  # geom_point(aes(x = first_letter, y = rank),
  #            position = position_jitter(width = 1, height = 2)) +
  labs(
    title = "IKEA",
    caption = "Source: Kaggle  |  Visualization: @charliegallaghr"
  ) + 
  scale_y_reverse(name = "RATIO OF VOWELS TO CONSONANTS") + 
  scale_x_discrete(name = "FIRST LETTER") +
  scale_size_continuous(range(2, 6)) + 
  scale_color_manual(values = c('#4b5c73ff', '#ffffff')) +
  guides(size = FALSE, color = FALSE) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#2c3643ff", color = NA),
    plot.title = element_text(
      family = "Roboto Bk", 
      size = 80, 
      color = 'white'),
    plot.caption = element_text(colour = "#4b5c73ff", size = 10),
    plot.margin = margin(20, 10, 20, 10),
    panel.background = element_rect(fill = "#2c3643ff", color = NA),
    axis.title.y = element_text(
      family = "Roboto Lt", 
      color = '#ffffff',
      angle = 90,
      size = 18),
    axis.title.x = element_text(
      family = "Roboto Lt", 
      color = "#ffffff",
      size = 18)
  )

svg(filename = "ikea.svg", width = 14, height = 11)
p2
dev.off()