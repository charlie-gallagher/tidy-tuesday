library(tidyverse)
library(ggforce)
library(systemfonts)
library(cowplot)


source('register_fonts.R')


get_thetas <- function(r, z, word) {
  left_bound <- atan2(y = sqrt(r^2 - z^2), x = -z)
  right_bound <- atan2(y = sqrt(r^2 - z^2), x = z)
  
  out <- seq(from = left_bound, to = right_bound, length.out = nchar(word))
  
  return(out)
}


# Function for reversing word
reverse_letters <- function(word) {
  out_word <- unlist(strsplit(word, split = ''))
  
  out_word <- out_word[length(out_word):1]
  
  return(out_word)
}


df <- tibble(
  thetas = get_thetas(1, 0.8, 'USPS'),
  letters = reverse_letters('USPS'),
  x = -cos(thetas),
  y = sin(thetas),
  angle = (thetas * 180 / pi) - 90
)

bg_df <- tibble(
  x = rep(c(-1.1, 1.1), times = 60),
  y = rep(seq(-1.1, 1.1, length.out = 60), each = 2),
  g = rep(1:60, each = 2)
)

fg_df <- tibble(
  x = c(6, 7, 8),
  y = c('a', 'b', 'c')
)


bg_graphic <- ggplot(df) + 
  geom_line(data = bg_df, aes(x = x, y = y, group = g), color = '#5c6392ff') +
  geom_text(aes(x = x, y = y, label = letters), angle = -df$angle, 
            size = 10, family = 'IBM Plex Sans Bold') +
  # geom_circle(data = tibble(x = 0, y = 0, r = 0.8),
  #                      aes(x0 = x, y0 = y, r = r),
  #             fill = NA) +
  geom_circle(data = tibble(x = 0.66, y = -0.66),
              aes(x0 = x, y0 = y, r = 0.3),
              fill = 'white') +
  annotate('text', x = 0.66, y = -0.66, label = 'By Charlie', family = 'IBM Plex Sans Bold') + 
  coord_fixed() + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#f3eddcff")
  )



fg_graphic <- fg_df %>% 
  ggplot() + 
  geom_col(aes(x = x, y = y, fill = y)) + 
  scale_fill_manual(values = c('red', 'white', 'blue')) +
  guides(fill = FALSE) +
  coord_polar() + 
  theme_void()



ggdraw(bg_graphic) + 
  cowplot::draw_plot(fg_graphic, scale = 0.8)
