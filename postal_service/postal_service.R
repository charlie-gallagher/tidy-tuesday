library(tidyverse)
library(systemfonts)
library(ggtext)

# You may need to change this to suit your fonts
# But dangit I wanted to use the systemfonts package
source('register_fonts.R')

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')



est <- post_offices %>% 
  group_by(established) %>% 
  summarize(n = n()) %>% 
  filter(established > 1700, established < 2020)

discontinued <- post_offices %>% 
  group_by(discontinued) %>% 
  summarize(n = n()) %>% 
  filter(discontinued > 1700, discontinued < 2020)


pos <- full_join(est, discontinued, by = c('established' = 'discontinued'),
                 suffix = c('_est', '_disc')) %>% 
  mutate(n_disc = -n_disc)


pos_text <- tibble(
  text = c('OPENED and <span style="color: #efefef">CLOSED</span>',
           'Post offices per year', '1764', '2002', '@charliegallaghr'),
  x = c(1883, 1883, 1765, 2000, 1995),
  y = c(7300, 6500, 250, 250, -6800),
  color = c('black', 'black', 'black', 'black', '#efefef'),
  size = c(9, 6, 4, 4, 3.5),
  family = c('Barlow Bold', 'Barlow', 'Barlow', 'Barlow', 'Barlow'),
  hjust = c(0.5, 0.5, 0, 1, 1)
)


p <- pos %>% 
  pivot_longer(cols = c(n_est, n_disc)) %>% 
  ggplot() + 
  # The first is the lower black box, the second the text highlight
  # Low black box's x limits expanded to remove phantom line
  # TODO: Convert this to a tribble for easier reading
  geom_rect(
    data = tibble(
      xmin = c(1760, 1899),
      xmax = c(2004, 1964.5),
      y = c(-7000, 7000),
      ymax = c(0, 7750)
    ),
    aes(xmin = xmin, xmax = xmax, ymin = y, ymax = ymax), fill = 'black', color = 'black'
  ) + 
  geom_richtext(data = pos_text,
            aes(x = x, y = y, label = text),
            color = pos_text$color, size = pos_text$size, family = pos_text$family,
            hjust = pos_text$hjust,
            fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) + 
  geom_col(aes(x = established, y = value, group = name, fill = name)) + 
  scale_fill_manual(values = c('#efefef', 'black')) + 
  scale_y_continuous(expand = c(0,0,0.05,0)) + 
  scale_x_continuous(expand = c(0,0)) + 
  # Need this to remove phantom white lines around edge of rectangle
  coord_cartesian(xlim = c(1764, 2002)) + 
  guides(fill = FALSE) + 
  theme_void() + 
  theme(
    plot.background = element_rect(color = NA, fill = '#efefef')
  )


ragg::agg_png('postal_service.png', width = 1500, height = 2250, res = 300)
p
dev.off()
