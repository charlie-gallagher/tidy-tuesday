library(tidyverse)
library(ggforce)
library(extrafont)

wa <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds')) 

wa <- wa %>% 
  mutate(
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    rating = as.numeric(rating)
  ) %>% 
  separate(col = length, into = c("len_miles", "len_type"),
           sep = " miles, ", remove = FALSE) %>% 
  mutate(
    len_miles = as.numeric(len_miles)
  )

wa_c <- wa %>%
  filter(len_type == "roundtrip") %>% 
  mutate(
    radius = len_miles / (pi * 2)
  )

reference_arcs <- tibble(
  ymax = c(2.5, 5, 8.5, 12.05),
  ymin = c(2.25, 4.5, 8, 11.7),
  xmin = 0,
  xmax = c(0.7, 1.5, 2, 2),
  curvature = -0.2
)

reference_lines <- tibble(
  xmin = c(0.35, 0.8, 1.1, 1.1),
  xmax = c(8, 9, 10, 10),
  y = c(2.45, 4.85, 8.35, 12)
)

reference_labels <- tibble(
  label = c("8 miles", "16 miles", "27 miles", "38 miles"),
  x = c(8, 9, 10, 10) + 0.1,
  y = c(2.45, 4.85, 8.35, 12)
)


p1 <- wa_c %>% 
  slice_head(n = 250) %>% 
  ggplot() + 
  geom_circle(aes(x0 = 0, y0 = radius, r = radius, color = len_miles)) + 
  geom_curve(data = reference_arcs[1,],
               aes(x = xmin, xend = xmax,
                   y = ymax, yend = ymin),
               color = '#bbbbbb', curvature = -0.25, size = 1) + 
  geom_curve(data = reference_arcs[2,],
             aes(x = xmin, xend = xmax,
                 y = ymax, yend = ymin),
             color = '#bbbbbb', curvature = -0.17, size = 1) + 
  geom_curve(data = reference_arcs[3,],
             aes(x = xmin, xend = xmax,
                 y = ymax, yend = ymin),
             color = '#bbbbbb', curvature = -0.12, size = 1) + 
  geom_curve(data = reference_arcs[4,],
             aes(x = xmin, xend = xmax,
                 y = ymax, yend = ymin),
             color = '#bbbbbb', curvature = -0.1, size = 1) + 
  geom_segment(data = reference_lines,
            aes(
              x = xmin, xend = xmax,
              y = y, yend = y
            ),
            color = '#bbbbbb') +
  geom_text(
    data = reference_labels,
    aes(x = x, y = y, label = label),
    color = '#bbbbbb', hjust = 0
  ) +
  labs(
    title = "WASHINGTON CIRCUIT TRAILS",
    subtitle = "Each circle represents a round-trip trail in Washington State.\nThe circumference represents the length of the trail.\nNote: Only a sample of 250 trails is represented.",
    caption = "Source: Washington Trails Association  |  Visualization: @charliegallaghr"
  ) +
  coord_fixed(xlim = c(-8, 12)) +
  scale_color_gradientn(colors = c("#0a244fff", 
                                   "#007727ff",
                                   "#f3ff48ff")) +
  guides(color = FALSE) +
  theme_void() +
  theme(
    text = element_text(
      family = "Roboto Lt", color = "white"
    ),
    plot.title = element_text(
      family = "Roboto Bk", size = 30
    ),
    plot.subtitle = element_text(
      size = 14
    ),
    plot.background = element_rect(fill = '#071220ff', color = NA),
    plot.margin = margin(10, 125, 10, 125)
  )




svg(filename = "washington.svg", width = 16, height = 18)
p1
dev.off()
