library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv',
                              col_types = cols(
                                year = col_double(),
                                school = col_character(),
                                seed = col_character(),
                                conference = col_character(),
                                conf_w = col_integer(),
                                conf_l = col_integer(),
                                conf_percent = col_double(),
                                conf_place = col_character(),
                                reg_w = col_integer(),
                                reg_l = col_integer(),
                                reg_percent = col_double(),
                                how_qual = col_character(),
                                x1st_game_at_home = col_character(),
                                tourney_w = col_integer(),
                                tourney_l = col_integer(),
                                tourney_finish = col_character(),
                                full_w = col_double(),
                                full_l = col_double(),
                                full_percent = col_double()
                              ))


tournament <- tournament %>% 
  mutate(
    tournament_level = case_when(
      tourney_finish == "1st" ~ 7,
      tourney_finish == "2nd" ~ 6,
      tourney_finish == "RSF" ~ 5,
      tourney_finish == "RF" ~ 4,
      tourney_finish == "NSF" ~ 3,
      tourney_finish == "N2nd" ~ 2,
      tourney_finish == "Champ" ~ 1
    ),
    seed_group = case_when(
      seed %in% c('1', '2', '3') ~ '1-3',
      seed %in% c('4', '5', '6') ~ '4-6',
      seed %in% c('7', '8', '9') ~ '7-9',
      seed %in% c('10', '11', '12') ~ '10-12',
      seed %in% c('13', '14', '15', '16') ~ '13-16'
    ),
    seed_group = factor(seed_group, 
                        levels = c('1-3','4-6','7-9','10-12','13-16'),
                        ordered = TRUE),
    seed_group = forcats::fct_explicit_na(seed_group)
  )

colors <- c('#a72167ff', '#1a2857ff', '#6ab2e2ff', 
            '#fdc52fff', '#17a85aff')

rect_1 <- data.frame(
  x1 = c(0.4),
  x2 = c(1.6),
  y1 = c(0, 4.5, 6.5, 8.5), 
  y2 = c(3.5, 5.5, 7.5, 9.5)
)


p <- tournament %>% 
  filter(seed_group != "(Missing)") %>% 
  ggplot() + 
  geom_rect(data = rect_1, aes(xmin = x1, xmax = x2,
                               ymin = y1, ymax = y2), 
            fill = "#cdb6a0ff") +
  geom_point(aes(x = 1, y = tournament_level + 2, color = seed_group), 
             position = position_jitter(width = 0.5, height = 0.25),
             size = 1) + 
  labs(title = "MARCH MADNESS",
       subtitle = "In the women's NCAA tournament, the top three seeded teams dominate\nthe final rounds.",
       caption = "Source: FiveThirtyEight  |  Visualization: @charliegallaghr") + 
  scale_color_manual(name = "Seed", values = colors) + 
  coord_polar(start = pi) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = '#bb9c87ff', color = "#bb9c87ff"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = 'Abril Fatface', size = 30,
                              margin = margin(0,0,0,0)),
    plot.subtitle = element_text(family = "Roboto Lt", 
                                 size = 13,
                                 margin = margin(0,0,0,0)),
    plot.caption = element_text(family = "Roboto Lt", 
                                 size = 8,
                                 margin = margin(0,0,0,0)),
    panel.background = element_blank(),
    legend.title = element_text(family = 'Roboto Lt'),
    legend.text = element_text(family = 'Roboto Lt'),
    legend.key.size = unit(2, 'mm'),
    legend.position = c(0.1, 0.85)
  )

svg(filename = 'wnba.svg')
p
dev.off()