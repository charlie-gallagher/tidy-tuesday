library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(countrycode)
library(cowplot)
library(extrafont)

# Get data -------
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

net <- unvotes %>% 
  left_join(select(roll_calls, rcid, importantvote, date), by = 'rcid') %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(issues, by = "rcid") %>% 
  filter(year %in% seq(1980, 2020) & 
           importantvote == 1 & 
           issue == "Human rights")


# Vector of OECD countries in ISO-2 character
oecd <- read_csv('oecd.csv') %>% 
  pull(LOCATION) %>% unique() %>% 
  countrycode("iso3c", "iso2c")

oecd <- oecd[!is.na(oecd)]

net <- filter(net, country_code %in% oecd)


# undirected graph data --------
full_net <- net %>%
  group_by_all() %>%
  summarize(other_country = net$country[net$country > country & net$rcid == rcid], 
            other_rcid = net$rcid[net$country > country & net$rcid == rcid],
            .groups = "drop") %>%
  left_join(select(net, country, rcid, vote), by = c("other_country" = "country", "other_rcid" = "rcid")) %>%
  rename(vote = vote.x, other_vote = vote.y) %>%
  filter(vote == other_vote) %>%
  select(country, other_country, vote, everything())



# Summarizing relationships for each country
full_net_sum <- full_net %>% 
  group_by(country, other_country, vote) %>% 
  summarize(n_together = n())

net_graph_sum <- graph_from_data_frame(full_net_sum, directed = FALSE,
                                       vertices = net_vertices) %>% 
  as_tbl_graph() %>% 
  activate(edges) %>% 
  mutate(vote = factor(vote, levels = c('yes', 'no', 'abstain')))


# Graphic -------

# Base graphic
p1 <- net_graph_sum %>% 
  activate(edges) %>% 
  filter(vote == 'no') %>% 
  ggraph(layout = 'kk') +
  geom_edge_fan(aes(alpha = n_together), edge_color = 'white') +
  geom_node_text(aes(label = name), color = 'white', family = 'Roboto Lt') + 
  labs(caption = "Source: Harvard Database    |    Visualization: @charliegallaghr  ") + 
  theme_void(base_family = 'Roboto Lt', base_size = 14) + 
  theme(
    text = element_text(family = 'Roboto Lt', color = 'white'),
    plot.margin = margin(0, 0, 10, 0),
    plot.background = element_rect(color = NA, fill = 'black'),
    legend.title = element_blank(),
    legend.position = c(0.745, 0.6825)
  )


# Final plotting
un_subtitle <- paste(
  "This network graph draws a line between every country in",
  "the OECD. The brightness of the line correlates with how",
  "often the countries connected by the edge voted 'No' on",
  "the same Human Rights issue since 1980.",
  sep = '\n'
)


p2 <- ggdraw(p1) + 
  draw_text("U.N. 'No' Votes", x = 0.8, y = 0.87, hjust = 0.5, color = 'white',
            size = 35, family = 'Bahnschrift') + 
  draw_text(un_subtitle, x = 0.8, y = 0.81, hjust = 0.5, color = 'white',
            size = 14, family = 'Roboto Lt', lineheight = 0.9) + 
  draw_line(x = c(0.63, 0.97), y = 0.85, color = 'white', size = 0.5) + 
  draw_text("Simultaneous 'No' Votes", x = 0.8, y = 0.72, color = 'white',
            size = 14, family = "Roboto Lt", hjust = 0.5)


ggsave('unvotes.png', plot = p2, height = 10, width = 11.25, scale = 1.3, type = 'cairo')

