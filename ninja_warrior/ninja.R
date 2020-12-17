library(dplyr)
library(ggplot2)
library(magrittr)

library(igraph)
library(tidygraph)
library(ggraph)

# Get data -------
rm_paren <- "\\s?\\(.*\\)\\s?"  # Remove parentheses with optional spaces


ninja <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv') %>% 
  mutate(
    round_stage = stringr::str_remove(round_stage, rm_paren),
    obstacle_name = stringr::str_remove(obstacle_name, rm_paren)
  )

stage_ordering <- c('Qualifying','Semi-Finals','Finals',
                    paste0('National Finals - Stage ', 1:4))

ninja <- mutate(ninja, 
                round_stage = factor(round_stage, levels = stage_ordering))


# Making the graph dataset --------

# Make edge dataset
e <- ninja %>% 
  group_by(season, round_stage) %>% 
  mutate(next_obstacle = lead(obstacle_name, default = "End")) %>% 
  group_by(obstacle_name, next_obstacle, round_stage) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  filter(next_obstacle != "End") %>% 
  mutate(
    sasuke = grepl("National Finals", round_stage)
  )

# Make vertex dataset 
#    Sort by mode of round_stage (factor)
#    Add an angle and hjust value


v <- ninja %>% 
  group_by(round_stage, obstacle_name) %>% 
  summarize(n = n()) %>% 
  group_by(obstacle_name) %>%
  arrange(obstacle_name) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(name = obstacle_name, 
         mode_stage = round_stage) %>% 
  arrange(mode_stage) %>% 
  mutate(
    n_row = n(),
    row_num = row_number(),
    angle = 90 - 360 * (row_num / n_row),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  ) %>% 
  select(-n_row,)

# Combine
# NOTE: igraph seems to drop factors, so I have to refactorize both nodes and edges
ninja_next <- graph_from_data_frame(e, vertices = v) %>% 
  as_tbl_graph() %>% 
  activate(edges) %>% 
  mutate(round_stage = factor(round_stage, levels = stage_ordering)) %>% 
  activate(nodes) %>% 
  mutate(mode_stage = factor(mode_stage, levels = stage_ordering))

# Layout
ninja_layout <- create_layout(ninja_next, 
                              layout = 'linear',
                              circular = TRUE, 
                              sort.by = mode_stage)
# graph
expand_amt <- rep(0.15, times = 2)

ggraph(ninja_next, layout = 'linear', circular = TRUE, sort.by = mode_stage) + 
  geom_node_point(size = 0.75) + 
  geom_node_text(aes(x = x*1.05, y = y*1.05, label = name, angle = angle, hjust = hjust),
                 size = 2.5, vjust = 0.3, color = 'white') +
  geom_edge_arc(aes(edge_width = n, color = round_stage, filter = sasuke), alpha = 0.4) + 
  scale_edge_color_brewer(type = 'qual', palette = 1) +
  scale_color_brewer(type = 'qual', palette = 1) + 
  scale_x_continuous(expand = expand_amt) +
  scale_y_continuous(expand = expand_amt) +
  guides(color = FALSE) + 
  coord_fixed() +
  theme_void() + 
  theme(
    text = element_text(color = 'white'),
    plot.background = element_rect(fill = 'black', color = NA)
  )

# save
ggsave("ninja.jpg", height = 10, width = 14, dpi = 320)

# Questions to answer: 
# 1. Do the text colors align with the proper mode? 



# Why is the right half yellow? Answer: Yellow is green with orange on top. 
ggraph(ninja_next, layout = 'linear', circular = TRUE, sort.by = mode_stage) + 
  geom_node_point(size = 0.75) + 
  geom_node_text(aes(x = x*1.05, y = y*1.05, label = name, angle = angle, hjust = hjust),
                 size = 2.5, vjust = 0.3, color = 'white') +
  geom_edge_arc(aes(filter = sasuke), color = grey(0.6), alpha = 0.3) + 
  geom_edge_arc(aes(color = round_stage, filter = !sasuke), alpha = 0.5) + 
  scale_edge_color_brewer(name = "Stage", type = 'qual', palette = 1) +
  scale_color_brewer(type = 'qual', palette = 1) + 
  scale_x_continuous(expand = expand_amt) +
  scale_y_continuous(expand = expand_amt) +
  guides(color = FALSE) + 
  coord_fixed() +
  theme_void() + 
  theme(
    text = element_text(color = 'white'),
    plot.background = element_rect(fill = 'black', color = NA)
  )
ggsave("ninja_1.jpg", height = 10, width = 14, dpi = 320)
