library(tidyverse)
library(extrafont)

loadfonts(device = 'win')

charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

charts_clean <- charts %>% 
  mutate(
    chart_position = as.integer(gsub("—", "", chart_position)),
    released_date = gsub(" \\(US\\)\\[51\\]", "", released),
    released_date = gsub(" \\(UK\\)\\[39\\]", 
                         "", released_date),
    released_date = lubridate::mdy(released_date),
    chart = factor(chart, 
                   levels = c('AUS','CAN','FRA','GER','IRE','JPN',
                              'NZ','SWE','UK','US'),
                   labels = c('AUSTRALIA','CANADA','FRANCE','GERMANY',
                              'IRELAND','JAPAN','NEW ZEALAND','SWEDEN',
                              'UK','US'))
  ) %>% 
  select(artist, title, released, released_date, everything()) %>% 
  filter(artist == "Taylor Swift")


p <- charts_clean %>% 
  ggplot() + 
  geom_col(aes(x = reorder(title,released_date), y = chart_position, 
               fill = reorder(title, released_date))) + 
  labs(title = "Taylor Swift Album Peak Rankings",
       subtitle = "Over her eight albums, Taylor has earned a global audience.",
       caption = "Data: Rosie Baillie and Dr. Sara Stoudt  |  Visualization: @charliegallaghr") + 
  facet_wrap(vars(chart), ncol = 2) + 
  scale_fill_manual(name = "ALBUMS (Chronological Order)", 
                    values = rep("#fff1aaff", 8)) + 
  scale_y_reverse(name = "Peak\nAlbum\nRanking",
                  breaks = c(1, 20, 40, 60, 80)) + 
  guides(fill = guide_legend(
    direction = 'horizontal',
    title.position = 'top',
    label.position = 'top',
    label.hjust = 0.5,
    nrow = 1,
    keywidth = 3
  )) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#54cddeff", color = NA),
    plot.margin = margin(10, 20, 10, 10),
    plot.title = element_text(family = 'Oswald SemiBold', face = 'bold',
                              color = "#fff1aaff",
                              size = 25,
                              hjust = 0.5),
    plot.subtitle = element_text(family = 'Roboto Lt',
                                 color = "#fff1aaff",
                                 size = 12,
                                 hjust = 0.5),
    plot.caption = element_text(family = "Roboto Bk",
                                color = "#97e1ebff",
                                size = 8,
                                margin = margin(10, 0, 0, 0)),
    panel.background = element_rect(fill = "#54cddeff", color = NA),
    panel.grid.major.y = element_line(color = "#97e1ebff", 
                                      linetype = 'solid',
                                      size = 1),
    panel.spacing.x = unit(20, units = 'pt'),
    panel.spacing.y = unit(30, units = 'pt'),
    axis.title.y = element_text(color = "#97e1ebff", 
                                margin = margin(0, 0, 5, 0),
                                hjust = 1,
                                vjust = 1),
    axis.text.y = element_text(color = "#97e1ebff", 
                               margin = margin(5, 5, 5, 5)),
    strip.text = element_text(color = '#fccad6ff', 
                              family = 'Roboto Bk', face = 'bold', 
                              size = 12),
    legend.position = 'top',
    legend.text = element_text(family = 'Roboto Bk', color = "#97e1ebff",
                               size = 8),
    legend.title = element_text(family = 'Roboto Bk', color = "#fccad6ff",
                                margin = margin(0,0,0,0)),
    legend.margin = margin(15, 0, 30, 0)
  )

svg(filename = 'taylor_swift.svg', width = 7, height = 11)
p
dev.off()