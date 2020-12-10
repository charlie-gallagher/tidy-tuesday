library(ggplot2)
library(dplyr)
library(magrittr)

library(igraph)
library(tidygraph)
library(ggraph)


women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

women <- women %>% 
  mutate(
    activist = grepl("Activist", role, ignore.case = TRUE),
    activist_type = case_when(
      activist ~ paste(category, "activist", sep = " "),
      !activist ~ NA_character_
    )
  )

# Three layers of edges

# Edge Layers
e1 <- women %>%
  mutate(from = "BBC 100 Women") %>% 
  select(from, to = category, category = category) %>% 
  unique()

e2a <- women %>% 
  filter(!activist) %>% 
  select(from = category, to = name, category = category)

e2b <- women %>% 
  filter(activist) %>% 
  select(from = category,
         to = activist_type,
         category = category)

e2 <- rbind(e2a, e2b) %>% 
  unique()

e3 <- women %>% 
  filter(activist) %>% 
  select(
    from = activist_type,
    to = name,
    category
  )

e <- rbind(e1, e2, e3)




# Now, I need a dataset that tells the level of each piece
v1 <- tibble(
  name = "BBC 100 Women",
  level = 1,
  category = NA,
  activist = NA,
  leaf = FALSE
)

v2 <- women %>% 
  select(name = category) %>% unique() %>% 
  mutate(level = 2, category = name, activist = NA, leaf = FALSE)

v3a <- women %>% 
  filter(!activist) %>%
  select(name, category, activist) %>% 
  mutate(level = 3, leaf = TRUE)

v3b <- women %>%
  filter(activist) %>% 
  select(name = activist_type, category, activist) %>% 
  unique() %>% 
  mutate(level = 3, leaf = FALSE)

v3 <- rbind(v3a, v3b)


v4 <- women %>% 
  filter(activist) %>% 
  select(name, category) %>% 
  mutate(level = 4, activist = TRUE, leaf = TRUE)


v <- rbind(v1, v2, v3, v4)

women_graph <- graph_from_data_frame(d = e, vertices = v)
women_graph <- tidygraph::as_tbl_graph(women_graph) %>% 
  activate(nodes) %>% 
  mutate(
    root = name == "BBC 100 Women"
  )

rm(list = c('v1','v2','v3','v3a','v3b','v4','e1','e2','e2a','e2b','e3'))



# Graphs
ggraph(women_graph, layout = 'tree') + 
  geom_node_point(aes(color = category, filter = leaf)) + 
  geom_edge_diagonal(aes(color = category)) +
  geom_node_text(aes(filter = leaf, label = name),
                 angle = -90, size = 3, hjust = 0, nudge_y = -.05) +
  geom_node_point(aes(filter = root, y = y + 0.3), color = 'black', size = 30) +
  scale_edge_color_brewer(type = 'qual', palette = 3) + 
  scale_color_brewer(type = 'qual', palette = 3) + 
  coord_cartesian(ylim = c(-1, 4)) +
  theme_minimal()

