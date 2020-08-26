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

# number of unique ingredients (text)
n_unique

n_unique %>% 
  ggplot() + 
  geom_col(aes(x = meal, y = n_unique, fill = meal))

# Top 5 ingredients for each meal
app_top_5 %>% 
  ggplot() + 
  geom_col(aes(x = reorder(meal, -n), y = n, fill = meal))
  # Reorder() isn't necessary here; I include it to show a good method of 
  # reordering bars. I pre-processed the data to save on typing.

entree_top_5 %>% 
  ggplot() + 
  geom_col(aes(x = meal, y = n, fill = meal))

dessert_top_5 %>% 
  ggplot() + 
  geom_col(aes(x = meal, y = n, fill = meal))

# It still feels like I don't have a complete graphic here. Maybe I should 
# experiment with iconography. 
