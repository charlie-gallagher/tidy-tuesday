library(tidyverse)
library(extrafont)
library(moments)
library(cowplot)

allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')




# Make dataset -----
some_numbers <- allNumbers %>%
  group_by(id) %>% 
  # Mutate grouped on ID
  mutate(skew = moments::skewness(lightness),
         first_num = max(numbers, na.rm = TRUE),
         last_num = min(numbers, na.rm = TRUE),
         mean_light = mean(lightness),
         median_light = median(lightness),
         weight = case_when(
           numbers == first_num ~ 100 * skew,
           numbers == last_num ~ (-100) * skew
         )
  ) %>% 
  filter(!is.na(weight),
         brand %in% c('SEPHORA COLLECTION', 'bareMinerals', 'Armani Beauty')) %>%  # Change to change data filtering
  ungroup() %>% 
  # Mutate ungrouped
  mutate(id = reorder(id, mean_light)) %>% 
  arrange(id) %>% 
  group_by(brand) %>% 
  mutate(
    y_place = rep(1:(0.5 * length(brand)), each = 2),
    y_place = y_place - median(y_place)
  ) %>% 
  ungroup()


# Extra data ----

# Background text
bg_text <- tibble(
  x = 0.1, 
  y = 0,
  brand = c('SEPHORA COLLECTION', 'bareMinerals', 'Armani Beauty'), 
  text = c('SEPHORA', 'BAREMINERALS', 'ARMANI')
)

# Key data
makeup_key_points <- tibble(
  x = c(0, 0.85, 0.25, 0.5),
  y = 6,
  brand = 'Armani Beauty'
)

makeup_key_line <- tibble(
  x = c(0, 0.85),
  y = 6,
  brand = 'Armani Beauty'
)

makeup_key_text <- tibble(
  text = c('Darkest\nShade', 'Lightest\nShade', 'mean', 'median'),
  x = c(0, 0.85, 0.25, 0.5),
  y = c(5.5, 5.5, 5.5, 5.5),
  brand = 'Armani Beauty'
)



# Plot ------
p1 <- some_numbers %>% 
  # Plot data
  ggplot() + 
  geom_line(aes(x = lightness, y = y_place, group = id), color = grey(0.75)) + 
  geom_point(aes(x = lightness, y = y_place, size = weight,), 
             shape = 'square', color = some_numbers$hex) + 
  geom_point(aes(x = mean_light, y = y_place)) +
  geom_point(aes(x = median_light, y = y_place), color = '#999999', fill = 'white',
             shape = 'circle fill') +
  geom_text(data = bg_text, aes(x = x, y = y, label = text),
            angle = 90, color = '#bbbbbb', hjust = 0.5, size = 17,
            family = 'Alegreya ExtraBold') + 
  # Key
  geom_line(data = makeup_key_line, aes(x = x, y = y)) + 
  geom_point(data = makeup_key_points, aes(x = x, y = y),
             shape = c('square', 'square', 'circle', 'circle fill'),
             color = c('#8a6a3d', '#f7e7d0', 'black', '#999999'),
             fill = c(NA, NA, NA, 'white'),
             size = c(3, 8, 2, 2)) +
  geom_text(data = makeup_key_text, aes(x = x, y = y, label = text),
            family = 'Montserrat Light', angle = c(0, 0, 90, 90), size = 3.5,
            lineheight = 0.9) +
  labs(
    title = 'SHADES OF MAKEUP',
    subtitle = "Exploring biases in available tones\nacross brands' makeup products",
    caption = 'Source: The Pudding Data    |    Visualization: @charliegallaghr'
  ) + 
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0.3, 0, 0.1)) + 
  scale_size_continuous(range = c(1, 8)) + 
  guides(size = FALSE) +
  facet_wrap(vars(brand)) + 
  theme_void() + 
  theme(
    plot.margin = margin(20, 10, 10, 10),
    plot.background = element_rect(color = NA, fill = '#fcfbfa'),
    plot.title = element_text(family = 'Montserrat Light', color = 'black',
                              size = 30, hjust = 0.5,
                              margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_text(family = 'Montserrat Light', color = 'black',
                                 size = 14, hjust = 0.5,
                                 margin = margin(5, 0, 30, 0)),
    plot.caption = element_text(family = 'Montserrat Light', size = 10, color = '#888888'),
    panel.spacing.x = unit(50, units = 'pt'),
    strip.text = element_blank()
  )


p2 <- ggdraw(p1) + 
  draw_text(text = 'Key', x = 0.16, y = 0.88, family = 'Montserrat Light') + 
  draw_line(x = c(0.05, 0.26), y = 0.87, color = '#999999', size = 0.35) + 
  draw_text(text = "A larger light square indicates\nmore available light shades than\ndark shades, and vice versa",
            x = 0.05, y = 0.85, family = 'Montserrat Light', size = 7, 
            hjust = 0, lineheight = 0.9) + 
  draw_line(x = c(0.24, 0.25, 0.25), y = c(0.85, 0.85, 0.815), size = 0.25, 
            color = '#999999')



ggsave('makeup_shades.png', plot = p2, height = 11, width = 8.5, type = 'cairo')
