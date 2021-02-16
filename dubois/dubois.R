library(tidyverse)
library(scales)
library(extrafont)

extrafont::loadfonts(device = 'win')


# Let's map this to the data
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
names(furniture) <- c('year', 'value')

# The length of the line is determined by the household value, so in all cases
# the end of the line is `value`. The beginning of the line is 0. 
# Line-length is split by revolutions. One revolution covers about half a million
# dollars or so. If delta-y is 1, x = sqrt(value^2 - 1). This is very close to 
# the value of value, so I will just use that

# Unsexy, poorly implemented construction of dataset
rev_val <- 800000

fur <- furniture %>% 
  group_by_all() %>% 
  summarize(
    rev = 1:ceiling(value / rev_val)
  ) %>% group_by_all() %>% 
  summarize(
    x = c(0, value)
  ) %>% 
  ungroup() %>% 
  mutate(
    x = case_when(
      x == 0 ~ 0,
      rev == 1 & x < rev_val ~ x,
      rev == 1 & x > rev_val ~ rev_val,
      rev == 2 & x > rev_val ~ x - rev_val
    ),
    y = case_when(
      rev == 1 & x == 0 ~ 2,
      rev == 1 & x != 0 ~ 2 - x / rev_val,
      rev == 2 & x == 0 ~ 1,
      rev == 2 & x != 0 ~ 1 - x / rev_val
    )
  )

# This is fine, but I still need to offset each y by a little
fur <- fur %>% 
  mutate(
    new_year = replace(year, year == 1899, 1900),
    y = y + (1887 - new_year) / 20
  )

# I just need some text now
furtext <- arrange(fur, year) %>% 
  filter(x == 0, rev == 1) %>% 
  mutate(
    text = paste0(year, '——$', scales::comma(value))
  )



# Plot
p1 <- ggplot() + 
  geom_line(data = fur, aes(x = x, y = y, group = interaction(year, rev),
                color = factor(year)), size = 4) + 
  geom_text(data = furtext, aes(x = rev_val - 5000, y = y, label = text),
            hjust = 1, size = 3.3, family = 'Roboto Th') + 
  labs(
    title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES.",
    caption = "Font: Olney by Kosal Sen\nSource:  Anthony Starks, Allen Hillery, and Sekou Tyler   |   Inspiration: W. E. B. Dubois   |   Visualization: @charliegallaghr"
  ) + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 2.61)) +
  scale_color_manual(values = c('#d9a9a2ff', '#acaab3ff', 
                                '#b99e84ff', '#f3b827ff',
                                '#ada08fff', '#bd4054ff')) + 
  guides(color = FALSE) +
  theme_void() + 
  theme(
    plot.margin = margin(5, 0, 10, 0),
    plot.background = element_rect(color = NA, fill = "#e5d8c7ff"),
    plot.title = element_text(family = "Olney", size = 15, hjust = 0.5),
    plot.caption = element_text(family = 'Roboto Lt', size = 8)
  )


# ggsave('dubois.png', height = 5, width = 4.6, scale = 1.45, type = 'cairo')

svg('dubois.svg', height = 7.25, width = 6.6)
p1
dev.off()


