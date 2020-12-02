library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(extrafont)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

agg_shelters <- shelters %>% 
  group_by(occupancy_date) %>% 
  summarize(
    occ = sum(occupancy, na.rm = TRUE),
    cap = sum(capacity, na.rm = TRUE),
    rel_cap = occ / cap
  ) %>% 
  ungroup() %>% 
  mutate(date = ymd(occupancy_date))

title_left <- ymd('2017-03-01')
subtitle_text <- "There are 168 homeless shelter programs in 68 locations in Toronto.\nThis graphic, inspired by Ed Hawkins’s “Warming Stripes,” shows daily\noccupancy of these homeless shelters from January 2017 to December\n2019. Occupancy is here defined as the share of total capacity of the\nhomeless programs that is occupied across the whole city for a given\nday. City-wide occupancy never fell below 90% of available beds."
title_bg <- data.frame(
  x1 = ymd('2017-02-01'),
  x2 = ymd('2018-04-01'),
  y1 = 0.9,
  y2 = 0.35
)

agg_shelters %>% 
  ggplot() + 
  geom_col(aes(x = date, y = 1, fill = rel_cap), width = 1) + 
  geom_rect(data = title_bg, aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1),
            fill = "#00000066") +
  geom_text(data = NULL, aes(x = title_left,
                                 y = 0.75, 
                                 label = "TORONTO\nHOMELESSNESS"),
            color = 'white', family = 'Roboto Bk', 
            fontface = "bold", size = 12, hjust = 0,
            lineheight = 0.8) +
  geom_text(data = NULL, aes(x = title_left, y = 0.62, label = subtitle_text),
            color = 'white', family = 'Roboto Lt', size = 3.5, hjust = 0, vjust = 1, lineheight = 0.8) +
  labs(caption = "Source: OpenDataToronto    |    Visualization: @charliegallaghr") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = c("#022c6bff", '#4093c7ff', "#dfecf7ff", "#ef3828ff", "#670005ff")) + 
  guides(fill = FALSE) + 
  theme_void() + 
  theme(
    text = element_text(family = 'Roboto Lt'),
    plot.margin = margin(20, 20, 10, 20),
    axis.text.x = element_text(color = grey(0.5)),
    axis.ticks.x = element_line(color = grey(0.5)),
    axis.ticks.length = unit(0.3, 'lines'),
    plot.caption = element_text(color = grey(0.4), hjust = 0.90)
  ) + 
  ggsave(filename = "toronto_heatbars.png", device = 'png', height = 5, width = 14, units = 'in',
         dpi = "print")
