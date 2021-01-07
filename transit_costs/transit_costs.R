library(tidyverse)
library(countrycode)
library(extrafont)

# Get data -----
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv', 
                                col_types = cols(
                                  e = col_character(),
                                  country = col_character(),
                                  city = col_character(),
                                  line = col_character(),
                                  start_year = col_character(),
                                  end_year = col_character(),
                                  rr = col_double(),
                                  length = col_double(),
                                  tunnel_per = col_character(),
                                  tunnel = col_double(),
                                  stations = col_double(),
                                  source1 = col_character(),
                                  cost = col_double(),
                                  currency = col_character(),
                                  year = col_double(),
                                  ppp_rate = col_double(),
                                  real_cost = col_character(),
                                  cost_km_millions = col_double(),
                                  source2 = col_character(),
                                  reference = col_character()
                                ))

# Cleaning up -------
transit_cost <- transit_cost %>% 
  mutate(
    start_year = case_when(
      start_year == '4 years' | start_year == '5 years' | 
        start_year == 'not start' ~ NA_character_,
      TRUE ~ start_year
    ),
    end_year = replace(end_year, end_year == 'x', NA_character_),
    tunnel_per = str_extract(tunnel_per, pattern = "\\d*.\\d*"),
    start_year = as.numeric(start_year),
    end_year = as.numeric(end_year),
    tunnel_per = as.numeric(tunnel_per)
  ) %>% 
  filter(!is.na(e))

# Merging in continent and summarizing by country --------
t_sum <- transit_cost %>% 
  # Make variables
  mutate(
    not_begun = start_year > 2020 | tunnel_per == 0,
    begun = start_year <= 2020 & tunnel_per < 100,
    finished = tunnel_per == 100
  ) %>% 
  # Summarize by country
  group_by(country) %>% 
  summarize(
    n_not_begun = sum(not_begun, na.rm = TRUE),
    n_begun = sum(begun, na.rm = TRUE),
    n_finished = sum(finished, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(n_not_begun, n_begun, n_finished)) %>% 
  mutate(name = factor(name, levels = c('n_not_begun', 'n_begun', 'n_finished'))) %>% 
  # Join in country names and continents
  left_join(select(codelist, iso2c, continent, country_name = country.name.en),
            by = c("country" = "iso2c")) %>% 
  mutate(
    continent = replace_na(continent, 'Europe'),
    country_name = replace_na(country_name, 'United Kingdom')
  )






# Plot ------
# I want to make lines on top that show continent divisions. I could use the 
# numeric version of the new `cont_value` factor to generate this. Let's start
# with just a line


t_sum <- t_sum %>% 
  # Make a variable to reorder country
  # Reason: I want to sort by both continent and 'value' (num of projects)
  # but 'reorder()' only takes a single numeric argument
  mutate(
    cont_value = 1000 * as.numeric(as.factor(continent)) + value,
    country = reorder(country, desc(cont_value)) # Makes factor and reorders
  )

t_sum_text <- t_sum %>% group_by(country, country_name) %>% summarize(
  y = sum(value, na.rm = TRUE)
)

continent_lines <- t_sum %>% 
  group_by(continent) %>% 
  summarize(
    x = min(as.numeric(country), na.rm = TRUE) - 0.3,
    xend = max(as.numeric(country), na.rm = TRUE) + 0.3,
    y = -10
  )

subtitle_text <- paste("China is the world leader in subway and railway projects. Of 537 transit",
                        "constructions recorded by the Transit Costs Project, 253 (47%) were in",
                        "China. This graphic shows the number of projects recorded for each", 
                        "country, broken down by stage of completion.", 
                       sep = '\n')

p1 <- t_sum %>% 
  ggplot() + 
  geom_col(aes(x = country, y = value, 
               group = country, fill = name), 
           color = '#dddddd', size = .25) + 
  geom_text(data = t_sum_text, aes(x = country, y = y, label = country_name),
            color = '#444444', angle = 90, hjust = 0, nudge_y = 3) +
  geom_segment(data = continent_lines,
            aes(x = x, xend = xend, y = y, yend = y), color = '#555555') +
  geom_segment(
    data = pivot_longer(continent_lines, cols = c(x, xend)),
    aes(x = value, xend = value, y = y, yend = y + 5),
    color = '#555555'
  ) +
  geom_text(data = continent_lines,
            aes(x = (x + xend) / 2, y = y-5, label = continent),
            color = '#555555', angle = c(90, rep(0, 3), 90),
            hjust = c(1, rep(0.5, 3), 1)) +
  labs(
    title = "Transit Projects Across the Globe",
    subtitle = subtitle_text,
    caption = "Source: Transit Cost Project    |    Visualization: @charliegallaghr"
  ) + 
  guides(fill = FALSE) +
  scale_y_continuous(name = "Projects", expand = c(0.15, 0)) +
  scale_fill_manual(values = grey(c(1, 0.4, 0))) +
  theme_minimal() +
  theme(
    text = element_text(color = 'white'),
    plot.background = element_rect(color = NA, fill = 'black'),
    plot.title = element_text(family = 'Times New Roman', size = 30),
    plot.subtitle = element_text(family = "Roboto Lt", size = 10),
    plot.margin = margin(15, 8, 5, 8),
    panel.grid = element_blank(),
    # panel.border = element_rect(color = 'white', fill = NA),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

# Add a legend
legend_df <- data.frame(
  x = 50,
  ymax = c(220, 200, 180),
  group = c('Completed', 'In process', 'Planned')
)

p1 + 
  geom_rect(data = legend_df, 
           aes(xmin = x, xmax = x + 1, ymin = ymax - 20, ymax = ymax),
           fill = grey(c(0, 0.4, 1)), color = '#dddddd', size = 0.25) + 
  geom_text(data = legend_df,
            aes(x = 49.5, y = 0.5 * (2 * ymax - 20), label = group),
            color = '#dddddd', hjust = 1, size = 3) + 
  ggsave('transit_costs.svg', height = 7, width = 11)
