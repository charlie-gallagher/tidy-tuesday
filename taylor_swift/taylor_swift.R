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
  # Point-lines
  geom_line(aes(x = reorder(title, released_date), y = chart_position, group = chart),
            color = "white", size = 1) +
  geom_point(aes(x = title, y = chart_position),
             color = 'white') + 
  # Text only for the first facet
  geom_text(data = filter(charts_clean, chart == "AUSTRALIA"), 
            aes(x = title, y = chart_position, label = title),
            angle = 90, hjust = 1, color = 'white', nudge_y = -5,
            family = 'Roboto Bk', size = 3) +
  facet_wrap(vars(chart), ncol = 2) +
  # Appearances
  labs(title = "Taylor Swift Album Peak Rankings",
       subtitle = "Over her eight albums, Taylor has earned a global audience.",
       caption = "Data: Rosie Baillie and Dr. Sara Stoudt  |  Visualization: @charliegallaghr") + 
  scale_y_reverse(name = "Peak\nAlbum\nRanking",
                  breaks = c(1, 20, 40, 60, 80)) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#54cddeff", color = NA),
    plot.margin = margin(10, 50, 10, 10),
    plot.title = element_text(family = 'Oswald SemiBold', face = 'bold',
                              color = "#fff1aaff",
                              size = 25,
                              hjust = 0.5),
    plot.subtitle = element_text(family = 'Roboto Lt',
                                 color = "#fff1aaff",
                                 size = 12,
                                 hjust = 0.5,
                                 margin = margin(0, 0, 20, 0)),
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
                              size = 12)
  )

svg(filename = 'taylor_swift.svg', width = 7, height = 11)
p
dev.off()