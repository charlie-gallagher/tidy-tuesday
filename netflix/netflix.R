library(tidyverse)
library(ggtext)
library(extrafont)
library(systemfonts)
library(cowplot)

source('register_fonts.R')

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

nfx <- netflix_titles %>% group_by(title, director) %>% 
  filter(type == 'Movie') %>% 
  summarize(
    individual_name = unlist(str_split(cast, ", "))
  )


bacon_flicks <- nfx %>% filter(individual_name == 'Kevin Bacon') %>% 
  pull(title)


first_removed_actors <- nfx %>% filter(title %in% bacon_flicks) %>% 
  pull(individual_name) %>% 
  unique()

first_removed_movies <- nfx %>% 
  filter(individual_name %in% first_removed_actors) %>% 
  pull(title) %>% 
  unique()


second_removed_actors <- nfx %>% filter(title %in% first_removed_movies) %>% 
  pull(individual_name) %>% 
  unique()

second_removed_movies <- nfx %>% 
  filter(individual_name %in% second_removed_actors) %>% 
  pull(title) %>% 
  unique()

third_removed_actors <- nfx %>% filter(title %in% second_removed_movies) %>% 
  pull(individual_name) %>% 
  unique()

third_removed_movies <- nfx %>% 
  filter(individual_name %in% third_removed_actors) %>% 
  pull(title) %>% 
  unique()


fourth_removed_actors <- nfx %>% filter(title %in% third_removed_movies) %>% 
  pull(individual_name) %>% 
  unique()

fourth_removed_movies <- nfx %>% 
  filter(individual_name %in% fourth_removed_actors) %>% 
  pull(title) %>% 
  unique()

fifth_removed_actors <- nfx %>% filter(title %in% fourth_removed_movies) %>% 
  pull(individual_name) %>% 
  unique()

fifth_removed_movies <- nfx %>% 
  filter(individual_name %in% fifth_removed_actors) %>% 
  pull(title) %>% 
  unique()


sixth_removed_actors <- nfx %>% filter(title %in% fifth_removed_movies) %>% 
  pull(individual_name) %>% 
  unique()


# Making indicators for each group
nfx <- nfx %>% 
  mutate(
    kevin_bacon = individual_name == 'Kevin Bacon',
    one_degrees = individual_name %in% first_removed_actors,
    two_degrees = individual_name %in% second_removed_actors,
    three_degrees = individual_name %in% third_removed_actors,
    four_degrees = individual_name %in% fourth_removed_actors,
    five_degrees = individual_name %in% fifth_removed_actors,
    six_degrees = individual_name %in% sixth_removed_actors
  )

# Keeping only the first time one of the degrees was true
final_nfx <- nfx %>% pivot_longer(cols = kevin_bacon:six_degrees, names_to = "degree") %>% 
  group_by(individual_name) %>% 
  filter(value) %>% 
  filter(row_number(value) == 1) %>% 
  arrange(individual_name)




# Plotting ----
# Probably only worth a simple plot, with Kevin Bacon's face at the middle
# of concentric circles populated by a dot for every actor that far removed. 

final_nfx <- final_nfx %>% 
  ungroup() %>% 
   mutate(
     x = runif(n = nrow(.)),
     y = runif(n = nrow(.)),
     y = case_when(
       degree == "kevin_bacon" ~ y,
       degree == "one_degrees" ~ y + 1.5,
       degree == "two_degrees" ~ y + 2.5,
       degree == "three_degrees" ~ y + 3.5,
       degree == "four_degrees" ~ y + 4.5,
       degree == "five_degrees" ~ y + 5.5,
       degree == "six_degrees" ~ y + 6.5,
       TRUE ~ y
     ),
     degree = factor(degree, levels = c('kevin_bacon', 'one_degrees', 'two_degrees',
                                'three_degrees', 'four_degrees', 'five_degrees',
                                'six_degrees'))
   )


dot_values <- c('black', '#006d5c99', '#00a0ab99', '#00d3e199', '#88f7ff99', '#dcfdff99', 'white')
nfx_subtitle_text <- paste(
  "Only six of Kevin Bacon's movies have been on Netflix, but even still I was",
  "able to connect 19,282 of the 23,050 film actors in this Netflix database to",
  "Kevin Bacon in six steps or less.",
  sep = "\n"
)

## Data plot ----
p1 <- final_nfx %>% 
  ggplot() + 
  geom_point(aes(x = x, y = y, fill = degree), size = ifelse(final_nfx$degree == 'kevin_bacon', 2, 2), color = '#88f7ff99', shape = 21) + 
  labs(
    title = 'Six degrees of <span style="color:#005862">Kevin Bacon</span>', 
    subtitle = nfx_subtitle_text,
    caption = 'Source: Kaggle    |    Visualization: @charliegallaghr'
  ) + 
  coord_polar() + 
  scale_fill_manual(values = dot_values) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(color = FALSE, fill = FALSE) + 
  theme_void() + 
  theme(
    plot.title = element_markdown(color = 'black', family = "IBM Plex Sans Light", size = 30),
    plot.subtitle = element_text(color = '#777777', family = 'IBM Plex Sans', size = 14),
    plot.caption = element_text(color = '#777777', family = 'IBM Plex Sans', size = 10),
    plot.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(10, 0, 10, 10)
  )

## Image plot ----
pimage <- ggdraw(p1) + 
  cowplot::draw_image('kevin-bacon-celebrity-mask.png', x = 0.515, y = 0.445, hjust = 0.5, vjust = 0.5,
                      scale = 0.07)

ragg::agg_png('netflix.png', height = 2411, width = 2200, res = 300)
pimage
dev.off()

