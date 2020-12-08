library(ggplot2)
library(dplyr)
library(magrittr)

library(igraph)
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
  select(from, to = category) %>% 
  unique()

e2a <- women %>% 
  filter(!activist) %>% 
  select(from = category, to = name)

e2b <- women %>% 
  filter(activist) %>% 
  select(to = activist_type,
         from = category)

e2 <- rbind(e2a, e2b) %>% 
  unique()

e3 <- women %>% 
  filter(activist) %>% 
  select(
    from = activist_type,
    to = name
  )

e <- rbind(e1, e2, e3)




# Now, I need a dataset that tells the level of each piece
v1 <- tibble(
  name = "BBC 100 Women",
  level = 1,
  category = NA,
  activist = NA
)

v2 <- women %>% 
  select(name = category) %>% unique() %>% 
  mutate(level = 2, category = NA, activist = NA)

v3a <- women %>% 
  filter(!activist) %>%
  select(name, category, activist) %>% 
  mutate(level = 3)

v3b <- women %>%
  filter(activist) %>% 
  select(name = activist_type, category, activist) %>% 
  unique() %>% 
  mutate(level = 3)

v3 <- rbind(v3a, v3b)


v4 <- women %>% 
  filter(activist) %>% 
  select(name, category) %>% 
  mutate(level = 4, activist = TRUE)


v <- rbind(v1, v2, v3, v4)

women_graph <- graph_from_data_frame(d = e, vertices = v)

rm(list = c('v1','v2','v3','v3a','v3b','v4','e1','e2','e2a','e2b','e3'))


# Graphs
ggraph(women_graph, layout = 'tree') + 
  geom_node_point(aes(color = category)) + 
  geom_edge_link()

ggraph(women_graph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = factor(level))) + 
  coord_fixed()

# Why is one of the circles not plotting properly? 
# Why doesn't the category aesthetic work with the circles?