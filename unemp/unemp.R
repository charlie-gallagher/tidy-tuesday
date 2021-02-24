library(tidyverse)
library(lubridate)
library(extrafont)

# Get data ------
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

e <- earn %>% 
  filter(race != "All Races", year == 2020) %>% 
  mutate(
    month = case_when(
      quarter == 1 ~ 3,
      quarter == 2 ~ 6,
      quarter == 3 ~ 9,
      quarter == 4 ~ 12
    ),
    race = reorder(race, -median_weekly_earn)
  ) %>% 
  group_by(race) %>% 
  summarize(median_weekly_earn = mean(median_weekly_earn)) %>% 
  ungroup()



# Polygon plot -------

## Make data -------
width <- 300

e_poly <- e %>% 
  mutate(
    x_raw = sqrt(median_weekly_earn^2 / 2),
    int = c(width * 2, width, 0)
  ) %>% group_by(race) %>% 
  summarize(
    x = c(0, x_raw, x_raw, 0),
    y = c(int + width, x_raw + width + int, x_raw + int, int),
    .groups = 'drop'
  )

## Colors and text --------
e_cols <- c(
  "bg" = "#20211bff",
  "title" = "#ebe0d0ff",
  "subtitle" = "#cf9f2d",
  "text" = "#ebe0d0ff",
  "asian" = "#26a676ff",
  "white" = "#176548ff",
  "black" = "#213c32ff"
)


e_text <- tibble(
  x = 0,
  y = c(width * 2.5, width * 1.5, width * 0.5),
  text = c(
    " ASIAN                                              $1,179",
    " WHITE                               $965",
    " BLACK                    $779"
  )
)
  
  
## Plot -----
e_poly %>% 
  ggplot() + 
  geom_polygon(aes(x = x, y = y, group = race, fill = race)) + 
  geom_text(data = e_text,
            aes(x = x, y = y, label = text),
            color = e_cols[["text"]], hjust = 0, angle = 28,
            family = "Abril Fatface", size = 12) +
  labs(
    title = "WAGES",
    subtitle = "Median weekly earnings in the US in 2020.",
    caption = "Source: BLS    |    Visualization: @charliegallaghr"
  ) + 
  scale_fill_manual(values = c(e_cols[["asian"]], e_cols[["white"]], e_cols[["black"]])) +
  scale_x_continuous(expand = c(0,0)) + 
  guides(fill = FALSE) +
  theme_void() + 
  theme(
    plot.margin = margin(25, 25, 10, 0),
    plot.background = element_rect(color = NA, fill = e_cols[["bg"]]),
    plot.title = element_text(family = "Abril Fatface", color = e_cols[["title"]],
                              hjust = 0.5, size = 60, margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_text(family = "Roboto Lt", hjust = 0.5, 
                                 color = e_cols[["subtitle"]], size = 12,
                                 margin = margin(0, 0, 60, 0)),
    plot.caption = element_text(family = "Roboto Lt", size = 10, 
                                color = e_cols[["title"]])
  )

ggsave('unemp.png', type = 'cairo', height = 10, width = 6.5)
