library(tidyverse)
library(janitor)

chop <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

# Prepare the data ----------------------

# List of ingredients by meal
foods <- list(
  app = pull(chop, appetizer),
  entree = pull(chop, entree),
  dessert = pull(chop, dessert)
) %>% 
  map(function(x) paste(x, collapse = ", ")) %>% 
  map(function(x) str_split(x, pattern = ", "))

app = foods[[1]][[1]]
entree = foods[[2]][[1]]
dessert = foods[[3]][[1]]

rm(foods) # No longer need this

# Number of duplicates
n_unique <- tibble(
  meal = c('app','entree','dessert'),
  n_unique = c(length(app[unique(app)]),
               length(entree[unique(entree)]),
               length(dessert[unique(dessert)]))
  ) %>% 
  arrange(desc(n_unique)) %>% 
  mutate(meal = factor(meal, meal))

# Top 5 most used ingredients for each meal
make_top_5 <- function(meal) {
  tibble(meal) %>% 
    tabyl(meal) %>% 
    arrange(desc(n)) %>% 
    head(n = 5) %>% 
    mutate(meal = factor(meal, meal))
}

app_top_5 <- make_top_5(app)
entree_top_5 <- make_top_5(entree)
dessert_top_5 <- make_top_5(dessert)

# Generate graphics ----------------------

# Colors
# bg: #d8281f
# orange: #f38720
# grey: #c1c1c1
# white: #ffffff


# number of unique ingredients (text)
n_unique

png(filename = 'pdf/n_unique.png', width = 500, height = 1000)
meal_text <- c('Appetizer','Entree','Dessert')
meal_num_text <- c('1,701', '1,666', '1,652')
n_unique %>% 
  ggplot() + 
  geom_col(aes(x = meal, y = n_unique),
           fill = "#ffffff", width = 0.75) + 
  geom_text(aes(x = meal, y = n_unique, label = meal_text),
            angle = 90, hjust = 1.05, color = "#f38720",size = 20,
            fontface = 'bold') +
  geom_text(aes(x = meal, y = n_unique, label = meal_num_text),
            angle = 90, hjust = 0, color = "#ffffff", size = 15,
            fontface = 'bold') +
  scale_y_continuous(expand = c(0,0), limits = c(0,1900),
                     breaks = c(500,1000,1500)) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#d8281f", color = "#d8281f"),
    panel.background = element_rect(fill = "#d8281f", color = "#d8281f"),
    axis.line.x = element_line(color = "#c1c1c1", size = 2),
    axis.line.y = element_line(color = "#c1c1c1", size = 2),
    axis.text.y = element_text(color = "#ffffff", 
                               size = 35, face = 'bold',
                               hjust = 1),
    plot.margin = margin(10,10,10,10)
  )
dev.off()

# Top 5 ingredients for each meal
chop_theme <- theme_void() + 
  theme(
    plot.background = element_rect(fill = "#d8281f", color = "#d8281f"),
    panel.background = element_rect(fill = "#d8281f", color = "#d8281f"),
    axis.line.x = element_line(color = "#c1c1c1", size = 2),
    axis.line.y = element_line(color = "#c1c1c1", size = 2),
    axis.text.y = element_text(color = "#ffffff", 
                               size = 45, face = 'bold',
                               hjust = 1),
    axis.title.y = element_text(color = "#ffffff", 
                                size = 45, face = 'bold',
                                hjust = 0.5, angle = 90),
    plot.margin = margin(10,10,10,10)
  )

png(filename = "pdf/app_top_5.png", width = 1500, 500)
app_top_5 %>% 
  ggplot() + 
  geom_col(aes(x = reorder(meal, -n), y = n),
           fill = "#f38720", width = 0.75) + 
  geom_text(aes(x = meal, y = n, label = meal),
            vjust = -0.5, color = "#ffffff",
            size = 15, fontface = 'bold') + 
  scale_y_continuous(breaks = c(2L, 4L, 6L, 8L),
                     name = "Episodes",
                     limits = c(0,10),
                     expand = c(0,0)) + 
  chop_theme
dev.off()

png(filename = "pdf/dessert_top_5.png", width = 1500, 500)
dessert_top_5 %>% 
  mutate(
    meal = replace(as.character(meal), c(4, 5), c('cream\ncheese', 'marshmallow\nspread'))
  ) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(meal, -n), y = n),
           fill = "#f38720", width = 0.75) + 
  geom_text(aes(x = meal, y = n, label = meal),
            vjust = -0.5, color = "#ffffff",
            size = 15, fontface = 'bold', lineheight = 0.75) + 
  scale_y_continuous(breaks = c(2L, 4L, 6L, 8L, 10L, 12L, 14L),
                     name = "Episodes",
                     limits = c(0,16),
                     expand = c(0,0)) + 
  chop_theme
dev.off()

png(filename = "pdf/entree_top_5.png", width = 1500, 500)
entree_top_5 %>% 
  mutate(
    meal = replace(as.character(meal), c(2,3), c('rainbow\nchard', 
                                                 'broccoli\nrabe'))
  ) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(meal, -n), y = n),
           fill = "#f38720", width = 0.75) + 
  geom_text(aes(x = meal, y = n, label = meal),
            vjust = -0.5, color = "#ffffff",
            size = 15, fontface = 'bold', lineheight = .75) + 
  scale_y_continuous(breaks = c(2L, 4L, 6L, 8L, 10L, 12L),
                     name = "Episodes",
                     limits = c(0,14),
                     expand = c(0,0)) + 
  chop_theme
dev.off()

# It still feels like I don't have a complete graphic here. Maybe I should 
# experiment with iconography. 
