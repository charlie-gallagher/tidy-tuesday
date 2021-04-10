library(tidyverse)
library(cowplot)
library(extrafont)

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

soy <- soybean_use %>% 
  filter(entity %in% c('Northern America', 'South America')) %>% 
  select(entity, year, processed)

p1 <- soy %>% 
  mutate(value_mil = round(processed / 1000000, digits = 1)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = value_mil, color = entity, size = entity)) +
  labs(
    subtitle = "in Millions of Pounds per Year",
    caption = "Source: Our World in Data    |    Visualization: @charliegallaghr"
  ) + 
  scale_x_continuous(limits = c(1960, 2018), breaks = c(1960, 1980, 2000)) +
  scale_y_continuous(expand = c(0 , 2, 0, 5)) +
  scale_size_discrete(range = c(1.2, 1.5)) + 
  scale_color_manual(values = c('#32bd0c55', '#32bd0cff')) + 
  guides(color = FALSE, size = FALSE) +
  theme_void(base_family = 'IBM Plex Sans Condensed') + 
  theme(
    text = element_text(color = '#999999'),
    plot.title.position = 'plot',
    plot.title = element_text(family = "Playfair Display Medium", color = '#dddddd',
                              size = 20),
    plot.subtitle = element_text(family = 'IBM Plex Sans Light', face = 'italic',
                                 size = 12, margin = margin(32, 0, 20, 0)),
    plot.caption = element_text(size = 10, margin = margin(20, 0, 0, 0)),
    plot.background = element_rect(color = NA, fill = '#36353b'),
    plot.margin = margin(25, 45, 12, 45),
    panel.grid.major.y = element_line(color = '#777777', linetype = '28'),
    axis.text = element_text(color = '#999999', margin = margin(0, 10, 0, 0))
  )

p_final <- ggdraw(p1) + 
  draw_line(x = c(0.222, 0.351), y = 0.92, size = 12, color = '#447d33ff') +
  draw_text("PROCESSED SOYBEAN PRODUCTION", x = 0.06, y = 0.92,
            family = "Playfair Display Medium", color = '#dddddd',
            size = 20, hjust = 0) + 
  draw_text("South America", x = 0.845, y = 0.75, family = 'IBM Plex Sans Light', 
            size = 11, hjust = 0, color = '#dddddd') + 
  draw_text("North America", x = 0.845, y = 0.53, family = 'IBM Plex Sans Light', 
            size = 11, hjust = 0, color = '#bbbbbb')

ggsave('deforestation.png', plot = p_final, type = 'cairo', height = 7.25, width = 11.5, scale = 0.88)


# Converting to a mobile-style graphic

p2 <- soy %>% 
  mutate(value_mil = round(processed / 1000000, digits = 1)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = value_mil, color = entity, size = entity)) +
  labs(
    subtitle = "in Millions of Pounds per Year",
    caption = "Source: Our World in Data    |    Visualization: @charliegallaghr"
  ) + 
  scale_x_continuous(limits = c(1960, 2018), breaks = c(1960, 1980, 2000)) +
  scale_y_continuous(expand = c(0 , 2, 0, 5)) +
  scale_size_discrete(range = c(1.2, 1.5)) + 
  scale_color_manual(values = c('#32bd0c55', '#32bd0cff')) + 
  guides(color = FALSE, size = FALSE) +
  theme_void(base_family = 'IBM Plex Sans Condensed') + 
  theme(
    text = element_text(color = '#999999'),
    plot.title.position = 'plot',
    plot.title = element_text(family = "Playfair Display Medium", color = '#dddddd',
                              size = 20),
    plot.subtitle = element_text(family = 'IBM Plex Sans Light', face = 'italic',
                                 size = 16, margin = margin(32, 0, 50, 0)),
    plot.caption = element_text(size = 10, margin = margin(20, 0, 0, 0)),
    plot.background = element_rect(color = NA, fill = '#36353b'),
    plot.margin = margin(25, 35, 12, 35),
    panel.grid.major.y = element_line(color = '#777777', linetype = '28'),
    axis.text = element_text(color = '#999999', size = 14, 
                             margin = margin(0, 10, 0, 0))
  )


p_mobile <- ggdraw(p2) + 
  draw_line(x = c(0.358, 0.585), y = 0.95, size = 12, color = '#447d33ff') +
  draw_text("PROCESSED SOYBEAN PRODUCTION", x = 0.07, y = 0.95,
            family = "Playfair Display Medium", color = '#dddddd',
            size = 23, hjust = 0) + 
  draw_text("South\nAmerica", x = 0.845, y = 0.755, family = 'IBM Plex Sans Light', 
            size = 14, hjust = 0, color = '#dddddd') + 
  draw_text("North\nAmerica", x = 0.845, y = 0.53, family = 'IBM Plex Sans Light', 
            size = 14, hjust = 0, color = '#bbbbbb')

ggsave('deforestation_mobile.png', plot = p_mobile, device = ragg::agg_png(),
       height = 11.5, width = 7.5, scale = 0.88)
