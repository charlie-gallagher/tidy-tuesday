library(tidyverse)
library(lubridate)
library(cowplot)
library(extrafont)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

landline_entities <- c("United States", "Mexico", "Canada")

phones <- landline %>% 
  left_join(
    select(mobile, code, year, mobile_subs),
    by=c("code","year")
  ) %>% 
  mutate(
    lag_landline = lag(landline_subs),
    diff_landline = landline_subs - lag_landline,
    lag_mobile = lag(mobile_subs),
    diff_mobile = mobile_subs - lag_mobile,
    net_phones = landline_subs + mobile_subs
  ) %>% 
  select(-landline_subs, -lag_landline, -mobile_subs, -lag_mobile) %>% 
  filter(entity %in% landline_entities)



phone_waterfall <- phones %>% 
  mutate(
    date = ymd(paste0(year, '01-01')),
    ymax = net_phones - diff_landline
  ) %>% 
  pivot_longer(cols = c(diff_landline, diff_mobile)) %>% 
  mutate(
    name = factor(name, levels = c('diff_mobile','diff_landline')), # Proper ordering
    xmin = case_when(
      name == 'diff_mobile' ~ date - weeks(15),
      name == 'diff_landline' ~ date
    ),
    xmax = case_when(
      name == 'diff_mobile' ~ date,
      name == 'diff_landline' ~ date + weeks(15)
    ),
    ymin = case_when(
      name == 'diff_mobile' ~ ymax - value,
      name == 'diff_landline' ~ net_phones
    )
  )


# Key
key_df <- tibble(
  entity = c('Mexico', 'Mexico'),
  name = c('Mobile','Landline'),
  bar_fill = c(TRUE, FALSE),
  ymax = c(155, 155),
  ymin = c(120, 140),
  xmin = ymd(c('1995-01-01', '1997-01-01')),
  xmax = ymd(c('1997-01-01', '1999-01-01'))
)

# Key segments
key_segments <- tibble(
  entity = c('Mexico'),
  name = c('Mobile', 'Landline'),
  x = ymd(c('1996-01-01', '1998-01-01')),
  xend = ymd(c('1996-01-01', '1998-01-01')),
  y = c(119, 139),
  yend = c(115, 130),
  labels = c("Green shows a\npositive change",
             "Red shows a\nnegative change")
)

# Dashed net segment
net_segment <- tibble(
  entity = c('Mexico'),
  x = ymd(c('1994-01-01')),
  xend = ymd(c('2001-01-01')),
  y = 140,
  yend = 140,
  labels = c("Net Change")
)


p1 <- phone_waterfall %>% 
  mutate(
    bar_fill = value > 0
  ) %>% 
  ggplot() + 
  # Main data
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                group = name, fill = bar_fill)) + 
  # Key
  geom_rect(data = key_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                group = name, fill = bar_fill)) +
  geom_text(data = key_df,
            aes(x = ymd(c('1996-01-01', '1998-01-01')), y = ymax,
                label = name),
            angle = 90, hjust = 0, lineheight = 0.8,
            size = 3.5, family = "Roboto", color = grey(0.4)) +
  # Key annotations
  geom_segment(data = key_segments, 
               aes(x = x, xend = xend, y = y, yend = yend,
                   group = name),
               color = grey(0.4)) +
  geom_text(data = key_segments, 
            aes(x = x, y = yend-2, label = labels),
            hjust = 0, vjust = 1, size = 3, color = grey(0.4),
            lineheight = 0.9,
            family = "Roboto") +
  geom_segment(data = net_segment,
              aes(x = x, xend = xend, y = y, yend = yend),
              linetype = 'dashed', color = grey(0.5)) +
  geom_text(data = net_segment,
            aes(x = x, y = y, label = labels),
            family = "Roboto", size = 3, color = grey(0.4),
            hjust = 1) +
  facet_wrap(vars(entity), nrow = 1) +
  # Other stuff
  labs(
    title = "North America is Mobile",
    subtitle = "This waterfall chart shows the relative increases and decreases\nin phone subscriptions in three North American countries. Each\nchange is broken down into the change in mobile subscriptions\n(left bar) and landline subscriptions (right bar)."
  ) + 
  scale_fill_manual(values = c('#ff5959','#04d14c')) + 
  scale_y_continuous(name = "Phone Subscriptions\nPer 100 Persons",
                     breaks = seq(25, 175, 25),
                     expand = c(0,0),
                     limits = c(0,175)) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_text(
      family = "Crimson Text",
      size = 30,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      family = "Roboto Lt",
      size = 12,
      hjust = 0.5
    ),
    plot.caption = element_text(
      family = "Roboto",
      size = 10,
      hjust = 0.9,
      color = grey(0.5)
    ),
    panel.spacing.x = unit(10, 'mm'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x = element_line(),
    axis.text.x = element_text(
      size = 12,
      color = grey(0.4)
    ),
    axis.title.x = element_blank(),
    axis.line.y = element_line(),
    axis.text.y = element_text(
      size = 12,
      color = grey(0.4)
    ),
    axis.title.y = element_text(
      color = grey(0.4)
    ),
    strip.text = element_text(
      family = "Roboto Lt",
      size = 12
    )
    
  )


# Sparkline sub-chart
landline_entities <- c("United States", "Mexico", "Canada")

# Basic dataset
basic_landline_df <- landline %>% 
  left_join(
    select(mobile, code, year, mobile_subs),
    by=c("code","year")
  ) %>% 
  filter(entity %in% landline_entities) %>% 
  mutate(
    date = ymd(paste0(year, '-01-01'))
  )

# Annotation data
annot2 <- tibble(
  entity = "Canada",
  x = ymd(c('2013-01-01', '2013-01-01')),
  y = c(95, 35),
  label = c("Mobile", "Landline")
)


p2 <- basic_landline_df %>% 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = landline_subs, ymax = mobile_subs,
                  fill = (landline_subs >= mobile_subs))) + 
  geom_line(aes(x = date, y = landline_subs), color = "#2ca25f") +
  geom_line(aes(x = date, y = mobile_subs), color = "#2ca25f") +
  geom_text(data = annot2,
            aes(x = x, y = y, label = label),
            family = "Roboto", color = grey(0.4),
            size = 3) +
  facet_wrap(vars(entity)) + 
  labs(
    caption = "Source: OurWorldInData.org  |  Visualization: @charliegallaghr" 
  ) +
  scale_fill_brewer(type = 'seq', palette = 2) +
  scale_y_continuous(name = "Phone\nSubscriptions", expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  guides(fill = FALSE) + 
  theme_void() + 
  theme(
    strip.text = element_blank(),
    plot.caption = element_text(
      family = "Roboto",
      size = 10,
      hjust = 0.9,
      color = grey(0.5),
      margin = margin(10, 0, 5, 0)
    ),
    axis.line.x = element_line(color = grey(0.4)),
    axis.title.y = element_text(
      family = "Roboto", color = grey(0.4),
      size = 8, hjust = 1
    )
  )






p3 <- cowplot::plot_grid(p1, p2, rel_heights = c(6, 1), nrow = 2,
                  align = "v")




svg(filename = "landline.svg", width = 16, height = 10)
p3
dev.off()
