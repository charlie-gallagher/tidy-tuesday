library(dplyr)
library(ggplot2)
library(magrittr)

library(igraph)
library(tidygraph)
library(ggraph)

library(extrafont)
library(g)
library(cowplot)

# Get data -------
rm_paren <- "\\s?\\(.*\\)\\s?"  # Remove parentheses with optional spaces

ninja <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv') %>% 
  mutate(
    round_stage = stringr::str_remove(round_stage, rm_paren),
    obstacle_name = stringr::str_remove(obstacle_name, rm_paren)
  )


e <- ninja %>% 
  filter(grepl("National Finals - Stage 1", round_stage)) %>% 
  group_by(season, round_stage) %>% 
  mutate(next_obstacle = lead(obstacle_name, default = "Goal")) %>% 
  ungroup() %>% 
  select(from = obstacle_name, to = next_obstacle, season, location, obstacle_order) %>% 
  mutate(from_name = from, to_name = to)


# Vertices
v <- ninja %>% 
  filter(grepl("National Finals - Stage 1", round_stage)) %>% 
  group_by(obstacle_name) %>% 
  summarize(n = n()) %>% 
  rbind(data.frame(obstacle_name = "Goal", n = 10))
  
  
ninja_next <- e %>% graph_from_data_frame(vertices = v) %>% as_tbl_graph() 

# Generating a series of PNGs for Image Magick

season <- 1:10
season_filenames <- paste0("png/seas_", sprintf("%02d", 1:10), '.png')

season_plots <- purrr::map(season, function(seas) {
  ninja_next %>% 
    ggraph(layout = 'sugiyama') + 
    geom_edge_link(aes(filter = season != seas,
                       start_cap = label_rect(from_name),
                       end_cap = label_rect(to_name)), edge_width = 0.5,
                   arrow = arrow(length = unit(4, 'mm'), angle = 10, type = 'closed'),
                   color = "#f3714dff") + 
    geom_edge_link(aes(filter = season == seas,
                       start_cap = label_rect(from_name),
                       end_cap = label_rect(to_name)), edge_width = 1,
                   arrow = arrow(length = unit(4, 'mm'), angle = 10, type = 'closed'),
                   color = "white") + 
    geom_node_text(aes(label = name, size = n), color = 'white') +
    geom_image(data = NULL, aes(x = 6, y = 11.25, image = 'brush_stroke_1.png'),
               size = 0.5) +
    geom_text(data = NULL, aes(x = 6, y = 11.6, label = paste("Season", seas)),
              color = 'white', size = 10, family = 'Roboto Lt') +
    geom_text(data = NULL, aes(x = 6, y = 11.2, label = "First Stage - National Finals"),
              color = 'white', size = 5, family = 'Roboto Lt') +
    labs(caption = "Source: Sasukepedia    |    Visualization: @charliegallaghr") + 
    scale_edge_color_brewer(type = 'seq', palette = 2) +
    scale_size(range = c(2, 6)) +
    guides(edge_color = FALSE, edge_width = FALSE, size = FALSE) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    theme_void() + 
    theme(
      text = element_text(color = 'white'),
      plot.background = element_rect(fill = '#df3514ff', color = NA),
      plot.margin = margin(0, 15, 0, 15),
      plot.caption = element_text(color = 'white', family = 'Roboto Lt', size = 10,
                                  hjust = 0.9, margin = margin(0, 0, 10, 0))
    ) + 
    draw_image("nw_logo_1.png", x = 7.5, y = -0.5, width = 4, height = 4) + 
    ggsave("test.png", height = 11, width = 8.5, dpi = 'retina')
})

# From system: magick convert -delay 80 *.png output.gif
