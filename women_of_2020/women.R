library(ggplot2)
library(dplyr)
library(magrittr)

library(igraph)
library(tidygraph)
library(ggraph)
library(extrafont)

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv') %>% 
  filter(name != "Unsung hero")

# Comment -----
# This graphic uses a tree graph made by hand from the BBC 100 women
# data. {igraph} uses two datasets, an edge set and a vertex set
# to construct its graphs. Both sets may contain attributes of the 
# vertices and edges (each in their respective dataset). 



# Making the {igraph} object --------
v1 <- data.frame(
  name = "BBC 100 Women",
  level = 1,
  category = NA,
  leaf = FALSE,
  root = TRUE
)

v2 <- women %>% 
  select(name = category) %>% unique() %>% 
  mutate(level = 2, category = name, leaf = FALSE, root = FALSE)

v3 <- women %>% 
  select(name, category) %>% 
  mutate(
    level = 3,
    leaf = TRUE,
    root = FALSE
  )

v <- rbind(v1, v2, v3)
rm(list = c('v1','v2','v3'))


e <- women %>% 
  mutate(from = "BBC 100 Women") %>% 
  select(from, to = category, category = category) %>% 
  unique() %>% 
  rbind(select(women, from = category, to = name, category = category)) %>% 
  arrange(from)

women_graph <- graph_from_data_frame(
  d = e,
  vertices = v
)

# Proper ordering of categories
category_sort_order <- c('All', 'Leadership','Creativity','Knowledge','Identity')

# Convert to tbl_graph and modify category
women_graph <- as_tbl_graph(women_graph) %>% 
  activate(edges) %>% 
  mutate(category = factor(category, levels = category_sort_order)) %>% 
  activate(nodes) %>% 
  mutate(category = factor(category, levels = category_sort_order))


# Generate data for graph objects -------
# Banners dataset derived from graph's ggraph dataset
xranges <- ggraph(women_graph, layout = 'tree')$data %>% 
  filter(level == 3) %>% 
  group_by(category) %>% 
  summarize(
    xmin = min(x) - 0.5,
    xmax = max(x) + 0.5
  ) %>% 
  mutate(
    ymin = -2,
    ymax = 3,
    category = toupper(category),
    category = factor(category, levels = toupper(category_sort_order))
  )

banner_colors <- c('#54306c', '#4a2b67', '#392263', '#2b165c')

# Graph ----------

ggraph(women_graph, layout = 'tree') + 
  # Background banners
  geom_rect(data = xranges, aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                fill = category)) + 
  # Nodes
  geom_node_point(aes(color = category, 
                      filter = leaf)) +
  # Names of women
  geom_node_text(aes(label = name, filter = leaf),
                 angle = -90, hjust = 0, size = 2.5, nudge_y = -0.05,
                 family = 'Roboto', color = '#b3b1daff') + 
  # Edges
  geom_edge_diagonal(aes(color = category)) + 
  # Categories
  geom_text(data = xranges, aes(x = (xmin + xmax) / 2, 
                                y = ymin, label = category),
            color = '#b3b1daff', family = 'Roboto Cn', fontface = 'bold', angle = -90, 
            hjust = 1, vjust = 0.6, nudge_y = 0.1, size = 13) +
  # BBC 100 Women title
  geom_text(data = NULL, 
            aes(x = 0, y = 2.9, label = "BBC 100\nWOMEN"),
            color = 'white', family = 'Roboto Bk', fontface = 'bold',
            size = 17, lineheight = 0.8, hjust = 0.5, vjust = 1) +
  # OF 2020 subtitle
  geom_text(data = NULL,
            aes(x = 0, y = 2.35, label = "O F   2 0 2 0"),
            color = 'white', family = 'Roboto Th', size = 12,
            hjust = 0.5, vjust = 1) +
  # Unsung hero
  geom_text(data = NULL, aes(x = 0, y = 2.05, label = "Unsung hero"),
            hjust = 0.5, size = 4,
            family = 'Roboto Lt', color = '#b3b1daff') +
  # caption
  geom_text(data = NULL, aes(x = 45, y = -1.95, label = "Source: BBC     |     Visualization: @charliegallaghr"),
            hjust = 1, size = 2.5, color = '#b3b1daff', family = "Roboto") +
  scale_x_continuous(expand = c(0,0)) +  # These bring the graph to the edges
  scale_y_continuous(expand = c(0,0)) +
  scale_color_brewer(type = "seq", palette = "Purples") +
  # Edges have their own scales vvv
  scale_edge_color_brewer(type = "seq", palette = "Purples") + 
  scale_fill_manual(values = banner_colors) +  
  guides(color = FALSE, fill = FALSE, edge_color = FALSE) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = '#2e0048ff', color = NA)
  ) + 
  ggsave('women_graph_imit.png', width = 8.5, height = 12, dpi = 300)


# # I always save to svg and convert in Inkscape; the quality is somewhat better
# # Assign the above graph (minus ggsave) to p1, then run
# svg(filename = 'women_graph.svg', width = 8.5, height = 12)
# p1
# dev.off()
