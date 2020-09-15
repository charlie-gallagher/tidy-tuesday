library(tidyverse)
library(janitor)
library(extrafont)

loadfonts(device = 'win')

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

ggkids <- kids %>% 
  filter(year == 2016, variable == "lib") %>% 
  arrange(desc(inf_adj_perchild)) %>% 
  mutate(
    n_kids = inf_adj / inf_adj_perchild,
    ymin = 0,
    ymax = inf_adj_perchild,
    xmax = cumsum(n_kids),
    xmin = lag(xmax, default = 0),
    xcenter = (xmax + xmin) / 2
  )

ggkids_text <- ggkids %>% 
  select(ymax, xcenter, state, n_kids) %>% 
  filter(state %in% c("California", "Texas", "New York", "District of Columbia")) %>% 
  mutate(
    xwidth = paste0(state, " (", as.character(round(n_kids / 1000), n = 2), "k)"),
    xwidth_y = ymax + 0.005
  )

ggkids_p <- ggkids %>% 
  mutate(
    highlight_state = case_when(
      state %in% c("California", "Texas", "New York", "District of Columbia") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                fill = highlight_state),
            color = "white", size = 0.2) + 
  geom_text(data = ggkids_text, aes(x = xcenter, y = xwidth_y, label = xwidth),
            angle = 0, hjust = c(0, 0.5, 0.5, 0.5), color = '#35e8a4',
            family = 'Roboto',
            fontface = 'bold', vjust = c(1, 0, 0, 0)) + 
  labs(title = "State Spending on Libraries",
       subtitle = "This Marimekko chart looks at the number of dollars spent on libraries per child\nin the U.S. Such ibrary funding contributes to educational resources, after-school\nprograms, and many other services libraries perform. The height of the bars is\ndetermined by its public expenditure on libraries, while the width of the bars is\ndetermined by the child population of the state.",
       caption = "Source: Urban Institute  |  Visualization: @charliegallaghr") + 
  scale_fill_manual(values = c(grey(0.3), '#35e8a4')) + 
  scale_y_continuous(name = "Public spending per child on libraries ($1,000s)",
                     expand = c(0,0)) + 
  scale_x_continuous(name = "State (child population)", expand = c(0.01, 0.01)) + 
  coord_cartesian(ylim = c(0,0.41)) + 
  guides(fill = FALSE) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(family = 'Roboto', face = 'plain'),
    plot.title = element_text(family = 'EB Garamond', size = 36),
    plot.subtitle = element_text(family = 'Roboto Lt', size = 14),
    plot.caption = element_text(family = 'EB Garamond', size = 10, 
                                color = grey(0.5)),
    axis.title.x = element_text(family = 'Roboto', color = grey(0.3)),
    axis.title.y = element_text(family = 'Roboto', color = grey(0.3)),
    axis.text.y = element_text(family = 'Roboto', color = grey(0.3))
  )

svg(filename = 'tidykids.svg', width = 12, height = 6)
ggkids_p
dev.off()

