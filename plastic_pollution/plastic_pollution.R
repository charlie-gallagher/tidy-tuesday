library(tidyverse)
library(extrafont)

# Read in data --------
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# Get volunteer and collection data
vol <- plastics %>% 
  filter(parent_company == "Grand Total" & country != 'EMPTY') %>% 
  select(country, year, num_events, volunteers, grand_total) %>% 
  group_by(country) %>% 
  summarize(
    season = '2019-2020',
    events = sum(num_events, na.rm = TRUE),
    volunteers = sum(volunteers, na.rm = TRUE),
    grand_total = sum(grand_total, na.rm = TRUE)
  )

vol_countries <- c(
  'United States of America',
  'Indonesia',
  'Philippines',
  'NIGERIA',
  "Taiwan_ Republic of China (ROC)"
)

# Top 4 countries, and US; fix names while I'm here
vol <- vol %>% 
  filter(country %in% vol_countries) %>% 
  mutate(
    country = case_when(
      country == "NIGERIA" ~ "Nigeria",
      country == "Taiwan_ Republic of China (ROC)" ~ "Taiwan",
      TRUE ~ country
    ),
    country = reorder(country, grand_total)
  )


# Generating circular clusters -------
# Circle generating function
make_circle_cluster <- function(n, country) {
  R <- sqrt(n / pi)
  r <- R * sqrt(runif(n / 100))
  theta <- runif(n / 100) * 2 * pi
  
  df_out <- data.frame(
    x = r * cos(theta),
    y = r * sin(theta),
    r = R,
    country = country
  )
}

make_df_circle_cluster <- function(n, country) {
  out <- lapply(seq_along(n), function(i) {
    make_circle_cluster(n[i], country[i])
  }) %>% 
    do.call(rbind, .)
  
  return(out)
}

# Some custom circle spacing
x0 <- data.frame(
  x0 = c(0, 190, 426, 746, 1138),
  country = c("United States of America",
              "Indonesia", 
              "Philippines",
              "Nigeria",
              "Taiwan")
)

# Make clusters dataset
vol_cluster <- make_df_circle_cluster(vol$grand_total, vol$country) %>% 
  left_join(vol, by = "country") %>% 
  left_join(x0, by = "country") %>% 
  mutate(
    country = reorder(country, grand_total),
    x = x + x0
  )


# Make titling dataset
# Needs to have: 
#   - country names at proper x-offset
#   - number of volunteers and amount of trash collected
#   - Vertical lines
r <- c(37.1, 65.1, 108.6, 160.1, 196)
vol_label <- x0 %>% 
  left_join(vol, by = 'country') %>% 
  mutate(
    y0 = r * -1 - 20,
    y1 = -275,
    country = replace(country, country == "United States of America", 
                      "United States\nof America"),
    country = toupper(country),
    statement = sprintf("%d volunteers\n%d pieces of trash", volunteers, round(grand_total, -3)),
    statement_y = c(-330, -310, -310, -310, -310)
  )


# Title and legend
subtitle_text <- "The Break Free from Plastic Movement is a global movement\nwhose goal is to put positive pressure on corporations to reduce\ntheir waste. Their annual Brand Audits sort trash and identify\nthe brand and type of plastic, among other things. This graphic\nexplores five different contributors to the project, how many\nvolunteers they've had in the past two years, and how much trash \nthey've audited."
title_and_legend <- data.frame(
  label = c("BREAK FREE FROM PLASTIC", subtitle_text, "= 100 pieces of trash"),
  y = c(375, 325, 300),
  x = c(-100, -100, 1070),
  size = c(9, 4, 3),
  hjust = c(0, 0, 0),
  vjust = c(1, 1, 0.5),
  family = c('Roboto', 'Roboto Lt', 'Roboto Lt')
)



# Plotting ---------
p1 <- ggplot(vol_cluster) + 
  geom_point(aes(x = x, y = y)) + 
  geom_segment(data = vol_label, aes(x = x0, y = y0, xend = x0, yend = y1),
               size = 0.25) +
  geom_text(data = vol_label,
            aes(x = x0, y = statement_y, label = statement), vjust = 1,
            family = "Roboto Lt", lineheight = 0.9) + 
  geom_text(
    data = vol_label,
    aes(x = x0, y = -280, label = country),
    vjust = 1, fontface = 'bold', family = "Roboto", lineheight = 0.9
  ) + 
  # Title, subtitle, legend text
  geom_text(
    data = title_and_legend, 
    aes(x = x, y = y, label = label),
    size = title_and_legend$size,
    hjust = title_and_legend$hjust,
    vjust = title_and_legend$vjust,
    family = title_and_legend$family,
    lineheight = 0.9
  ) +
  geom_point(data = NULL, aes(x = 1060, y = 299)) +
  labs(
    caption = "Source: Break Free from Plastic    |    Visualization: @charliegallaghr"
  ) +
  scale_y_continuous(expand = c(0.1, 0, 0.05, 0)) + 
  scale_x_continuous(expand = c(0,0)) +
  coord_fixed() + 
  theme_void(base_family = "Roboto Lt") + 
  theme(
    plot.margin = margin(5, 20, 10, 30),
    plot.caption = element_text(size = 10)
  )

ggsave('plastic_pollution.png', p1, width = 12, height = 6.5, scale = 1.1)

svg('plastic_pollution.svg', width = 13, height = 7)
p1
dev.off()


