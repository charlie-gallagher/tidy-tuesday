library(tidyverse)

furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
names(furniture) <- c('year', 'value')

# CHANGE REV_VAL TO 200,000
rev_val <- 800000

fur <- furniture %>% 
  # Expand into two observations per revolution per year
  group_by_all() %>% 
  summarize(
    rev = 1:ceiling(value / rev_val)
  ) %>% 
  group_by_all() %>% 
  summarize(
    x = c(0, value)  # Placeholders
  ) %>% 
  ungroup() %>% 
  
  # Generate x and y values
  mutate(
    # Two useful values
    rem = x - (rev_val * rev),  # Remaining value
    ring = max(rev) - rev + 1,  # Which revolution (counting from outside)
    
    # x and y values
    x = case_when(
      x == 0 ~ 0,
      rem > 0 ~ rev_val,
      rem < 0 ~ x - (rev_val * (rev - 1))
    ),
    y = case_when(
      x == 0 ~ as.numeric(ring),
      x == rev_val ~ ring - 1,
      TRUE ~ ring - (x / rev_val)
    ),
    
    # Shift lines so they don't overlap
    new_year = replace(year, year == 1899, 1900),
    y = y + ((1887 - new_year) / 30)
  )





# Set rev_val = 200000
ggplot() + 
  geom_line(data = fur, aes(x = x, y = y, group = interaction(year, rev),
                            color = factor(year)), size = 3) + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 8.4)) +
  scale_color_manual(values = c('#d9a9a2ff', '#acaab3ff', 
                                '#b99e84ff', '#f3b827ff',
                                '#ada08fff', '#bd4054ff')) + 
  guides(color = FALSE) +
  theme_void()

ggsave('ex1.png', height = 10, width = 10)


# Set rev_val = 400000
ggplot() + 
  geom_line(data = fur, aes(x = x, y = y, group = interaction(year, rev),
                            color = factor(year)), size = 4) + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 4.4)) +
  scale_color_manual(values = c('#d9a9a2ff', '#acaab3ff', 
                                '#b99e84ff', '#f3b827ff',
                                '#ada08fff', '#bd4054ff')) + 
  guides(color = FALSE) +
  theme_void()

ggsave('ex2.png', height = 10, width = 10)


# Set rev_val = 800000
ggplot() + 
  geom_line(data = fur, aes(x = x, y = y, group = interaction(year, rev),
                            color = factor(year)), size = 4) + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 2.4)) +
  scale_color_manual(values = c('#d9a9a2ff', '#acaab3ff', 
                                '#b99e84ff', '#f3b827ff',
                                '#ada08fff', '#bd4054ff')) + 
  guides(color = FALSE) +
  theme_void()

ggsave('ex3.png', height = 10, width = 10)
