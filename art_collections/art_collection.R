library(tidyverse)
library(lubridate)
library(extrafont)
library(cowplot)

# Read in data ------
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

# Extract country of birth ------
artists <- artists %>% 
  mutate(
    birth_country = case_when(
      grepl(",", placeOfBirth) ~ stringr::str_extract(placeOfBirth, "(?<=, ).*$"),
      TRUE ~ placeOfBirth
    )
  )

artwork <- artwork %>% 
  left_join(select(artists, id, birth_country), by = c("artistId" = "id"))


# Make master dataset ----------

# Make year groups (decades)
artwork <- artwork %>% 
  mutate(
    year_group = cut(acquisitionYear, breaks = seq(1819.5, 2020.5, 10),
                     labels = paste0(seq(1820, 2010, 10), "'s"),
                     ordered_result = TRUE)
  )



# First year a country was acquired
first_years <- artwork %>% 
  group_by(birth_country) %>% 
  summarize(first_year = min(acquisitionYear, na.rm = TRUE)) %>%
  filter(!is.na(birth_country)) %>% 
  arrange(first_year) %>% 
  mutate(
    x = rep(1:11, times = 12)[1:129],
    y = rep(1:12, each = 11)[1:129],
    year_group = cut(first_year, breaks = seq(1819.5, 2020.5, 10),
                     labels = paste0(seq(1820, 2010, 10), "'s"),
                     ordered_result = TRUE)
  )

# Fully interacted country/year dataset
full_decade_country <- expand.grid(
  list(country = unique(artists$birth_country), 
       decade = unique(artwork$year_group))
) %>% 
  arrange(decade, country) %>% 
  filter(!is.na(decade))

# Reduce to only those countries that have appeared before
master <- full_decade_country %>% 
  left_join(first_years, by = c("country" = "birth_country")) %>% 
  filter(decade >= year_group & !is.na(country)) %>% 
  arrange(decade, first_year)

# Presence in a given year
given_decade <- artwork %>% group_by(year_group, birth_country) %>% 
  summarize(present = TRUE)

master <- master %>% 
  left_join(given_decade, by = c("decade" = "year_group", 
                                 "country" = "birth_country")
  ) %>% 
  mutate(present = replace_na(present, FALSE),
         first_time = decade == year_group,
         status = case_when(
           present & first_time ~ "new",
           present & !first_time ~ "again",
           !present ~ "old"
         ),
         status = factor(status, levels = c("new", "again", "old"))
  )



# Adding background grid -------
full_decade_country <- full_decade_country %>% mutate(present = TRUE) %>% 
  left_join(first_years, by = c("country" = "birth_country"))


# Plotting -------


col <- c(
  "bg" = "#1d2424",
  "text" = "#dcdcdd",
  "new" = "#d0c3bd",
  "again" = "#437571",
  "old" = "#324242"
)


# Plot the data
data_plot <- master %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(data = full_decade_country,
             size = 0.5, color = col['old']) +
  geom_point(aes(color = status), size = 3) + 
  labs(caption = "Source: Tate Collection    |    Visualization: @charliegallaghr") +
  scale_color_manual(name = "Status", values = col[3:5]) +
  guides(color = FALSE) +
  facet_wrap(vars(decade)) + 
  theme_void(base_family = 'Roboto Lt') + 
  theme(
    text = element_text(color = col['text']),
    plot.margin = margin(25, 100, 25, 100), 
    plot.background = element_rect(fill = col['bg'], color = NA),
    plot.caption = element_text(size = 13, margin = margin(50, 0, 0, 0)),
    panel.spacing.x = unit(35, 'pt'),
    panel.spacing.y = unit(55, 'pt'),
    strip.text = element_text(size = 12)
  ) 

# Text
# Pretending there's a 4x20 grid
art_text <- tibble(
  x = c(0.5, 1.4, 1.4, 12, 13.7),
  y = c(4, 4, 2.3, 3.75, 4),
  label = c("TATE", 
            "Cumulative International\nAcquisitions by Decade",
            "Today, the Tate Collection contains art from 129\ncountries. This graphic explores the gradual inter-\nnationalization of the collection. Each point in the\ngrid represents a country, arranged in the order in\nwhich artworks from each country were first\naquired.",
            "Key:",
            "Once a work of art from a country has been\nacquired, that country's circle is filled in."),
  angle = c(90, 0, 0, 0, 0),
  family = c("Arial Black", "Roboto Lt", "Roboto Lt", "Roboto", "Roboto Lt"),
  fontface = c("plain", "plain", "plain", "plain", "plain"),
  size = c(19, 11, 5, 10, 4.5),
  hjust = c(1, 0, 0, 0, 0),
  vjust = c(0.5, 1, 1, 0.5, 1)
)

# Plot the titles and legend
art_legend <- tibble(
  x = c(13.5, 16, 18.5),
  y = c(2.25),
  color = col[3:5],
  label = c("First acquisition", "Acquired again", "Previously Acquired")
)


art_text_plot <- art_text %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_text(aes(label = label), 
            color = col['text'], angle = art_text$angle, size = art_text$size, 
            family = art_text$family, fontface = art_text$fontface,
            hjust = art_text$hjust, vjust = art_text$vjust, lineheight = 0.9) + 
  geom_point(data = art_legend, size = 25, color = art_legend$color) + 
  geom_text(data = art_legend, aes(label = label, y = 1.25), 
            family = 'Roboto Lt', size = 4, color = col['text'], hjust = 0.5) +
  xlim(c(0,20)) + ylim(c(0, 4)) +
  theme_void(base_family = 'Roboto Lt') + 
  theme(
    plot.margin = margin(25, 40, 25, 40),
    plot.background = element_rect(fill = col['bg'], color = NA)
  )


# Combine
plot_grid(art_text_plot, data_plot, nrow = 2, rel_heights = c(1, 4)) + 
  ggsave('art_collection.png', height = 16, width = 13)
