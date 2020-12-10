library(ggplot2)
library(dplyr)
library(magrittr)

library(ggforce)
library(ggimage)
library(magick)
library(extrafont)
library(cowplot)

# *******************
# *******************
# DEFUNCT (but works)
# *******************
# *******************

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Image processing with magick -----

## Converting all images --------
img_crop <- image_read('crop_rect.png')
imgs <- women$img

img_list <- lapply(imgs, FUN = function(x) image_read(x)) %>% 
  lapply(function(x) image_flatten(c(x, img_crop)))


# Writing all images
filenames <- paste0("./tmp/img_", sprintf("%03d", 1:100), '.png')

for (i in 1:100) {
  image_write(img_list[[i]], path = filenames[i], format = 'png')
}
rm(i)

# Adding images to dataset --------
women <- women %>% cbind(tibble(img_files = filenames))



# Cowplot to the rescue? --------
df <- data.frame(
  x = c(rep(c(1, 3, 5, 2, 4, 6), 16), c(1, 3, 5)),
  y = rep(1:33, each = 3),
  img = women$img_files[2:100],
  name = women$name[2:100],
  role = women$role[2:100]
) %>% 
  mutate(laby = y - 0.3)


dfslice <- function(range) {
  plot <- df %>% 
    slice(range) %>% 
    ggplot() +
    geom_image(aes(x = x, y = y + 0.3, image = img), size = 0.14) +
    geom_text(aes(x = x, y = laby, label = name),
              size = 3, hjust = 0.5, color = '#698cafff', family = "Roboto Lt") +
    geom_text(aes(x = x, y = laby - 0.15, 
                  label = role),
              size = 2.5, hjust = 0.5, color = '#698cafff', family = 'Roboto Lt') + 
    theme_void()
  return(plot)
}


p1 <- dfslice(1:18)
p2 <- dfslice(19:36)
p3 <- dfslice(37:54)
p4 <- dfslice(55:72)
p5 <- dfslice(73:90)
p6 <- dfslice(91:100) + coord_cartesian(ylim = c(28, 33.5))

women_circles <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3)

