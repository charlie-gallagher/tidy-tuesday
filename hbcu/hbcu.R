library(tidyverse)
library(ggtext)
library(extrafont)

hbcu <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  janitor::clean_names()


# Make datasets -------
hbcu_long <- hbcu %>% 
  select(year, total_enrollment, x4_year, x2_year) %>% 
  pivot_longer(cols = x4_year:x2_year) %>% 
  mutate(
    share = value / total_enrollment
  )

hbcu_ends <- hbcu_long %>% 
  filter(year %in% c(1976, 2015)) %>% 
  mutate(
    xmin = c(1970.25, 1970.25, 2015.75, 2015.75),
    ymin = case_when(name == 'x2_year' ~ lag(share),
                     TRUE ~ 0),
    ymax = share + ymin,
    year = case_when(year == 2015 ~ 2020.75, TRUE ~ 1975.25)
  )

hbcu_lines <- tibble(
  x = c(1970.25, filter(hbcu_long, name == "x4_year")$year, 2020.75),
  y = c(0.928, filter(hbcu_long, name == "x4_year")$share, 0.874)
)

# Extra text df
t <- tibble(
  text = c(
    "1976", "2015",
    "7.2%", "12.6%",
    "92.8%", "87.4%",
    "Two-year colleges","Four-year colleges\nand universities",
    "Share of attendance at each school type"
  ),
  x = c(
    rep(c(1972.75, 2018.25), 3),
    2014, 2014,
    1995.5
  ),
  y = c(
    1.02, 1.02,
    0.97, 0.94,
    0.45, 0.43,
    0.95, 0.43,
    1.02
  ),
  family = c(
    "Roboto Lt", "Roboto Lt", 
    "Roboto", "Roboto",
    "Roboto", "Roboto",
    "Roboto Lt", "Roboto Lt",
    "Roboto"
  ),
  face = "plain",
  hjust = c(
    rep(0.5, 6),
    1, 1,
    0.5
  ),
  size = c(4.5, 4.5, 4, 4, 4, 4, 5, 5, 4),
  color = c(
    "#52626aff", "#52626aff",
    "white", "white",
    "white", "white",
    "white", "white",
    "#728892ff"
  )
)


# Auxiliary elements -----
hbcu_subtitle <- paste(
  "Historically black colleges and universities (HBCUs) have experienced",
  "a rise in the attendance of two-year colleges. This is according to data",
  "gathered by the National Center for Education Statistics (NCES).",
  sep = '\n'
)


hbcu_bright <- c('#49575dff', '#fb0a76ff')
hbcu_pale <- c('#fd7fa6ff', '#728892ff')
hbcu_titles <- tibble(
  x = 1976, y = 1.25, lab = '<span style=color:#fb0a76ff>Two-year colleges</span> gain\nground among HBCUs'
)

p1 <- ggplot(hbcu_long) + 
  geom_rect(data = hbcu_ends, aes(xmin = xmin,
                                  xmax = year,
                                  ymin = ymin, ymax = ymax),
            fill = rep(hbcu_bright, 2)) +
  geom_area(aes(x = year, y = share, group = name, fill = name)) + 
  geom_line(data = hbcu_lines, aes(x = x, y = y), 
            color = 'white', size = 2) +
  geom_text(data = t, aes(x = x, y = y, label = text),
            family = t$family, fontface = t$face, size = t$size,
            color = t$color, hjust = t$hjust) +
  labs(title = '<span style=color:#fb0a76ff>Two-year colleges</span> gain\nground among HBCUs',
       subtitle = hbcu_subtitle,
       caption = "Source: NCES    |    Visualization: @charliegallaghr") +
  scale_fill_manual(values = hbcu_pale) + 
  scale_y_continuous(expand = c(0,0, 0, 0.05)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(fill = FALSE) + 
  theme_void(base_family = 'Roboto Lt') + 
  theme(
    plot.margin = margin(15, 60, 15, 60),
    plot.title = element_markdown(family = 'Roboto', face = 'bold', size = 20),
    plot.subtitle = element_text(size = 12, margin = margin(2, 0, 30, 0),
                                 color = "#52626aff"),
    plot.caption = element_text(family = 'Roboto Lt', size = 9)
  )

ggsave('hbcu.png', plot = p1, height = 8, width = 10.5)

