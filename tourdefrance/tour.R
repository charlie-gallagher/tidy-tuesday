library(tidyverse)
library(lubridate)
library(extrafont)

tdf <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

extrafont::loadfonts(device = 'win')

# Decade function
to_decade <- function(date) {
  year = lubridate::year(date)
  decade = year - (year %% 10)
  decade_date = ymd(paste0(decade, '-01-01'))
  return(decade_date)
}


tdf <- tdf %>% 
  mutate(
    rel_margin = time_margin / time_overall,
    decade_start = to_decade(start_date)
  )


tdf_decade <- tdf %>% 
  group_by(decade_start) %>% 
  summarize(
    mean_time_margin = mean(rel_margin, na.rm = TRUE),
    decade_end = decade_start + years(10) - days(1)
  ) %>% 
  unique()

tdf_decade_spread <- tdf %>% 
  group_by(decade_start) %>% 
  summarize(
    max_rel_margin = max(rel_margin, na.rm = TRUE),
    min_rel_margin = min(rel_margin, na.rm = TRUE)
  ) %>% 
  mutate(
    decade_end = decade_start + years(10) - days(1)
  )

tdf_label <- tdf_decade %>% 
  mutate(
    label_subst = substr(year(decade_start), 3, 4),
    label = paste0("'", label_subst)
  )


tdf_decade %>% 
  ggplot() + 
  geom_rect(
    data = tdf_decade_spread,
    aes(xmin = decade_start, xmax = decade_end,
        ymin = min_rel_margin, ymax = max_rel_margin),
    fill = "#05f2cf33"
  ) +
  geom_segment(aes(x = decade_start, xend = decade_end,
                   y = mean_rel_margin, yend = mean_rel_margin),
               size = 2, color = "#05f2cfbb") +
  geom_point(data = tdf, aes(x = start_date, y = rel_margin)) + 
  geom_text(data = tdf_label, aes(x = decade_start + years(5), 
                                   y = 0.037, 
                                   label = label),
            family = "Roboto Lt") +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "10 years", expand = c(0,0)
    ) +
  scale_y_continuous(limits = c(0,0.04), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )



# Raw hours won by over time
tdf_decade <- tdf %>% 
  group_by(decade_start) %>% 
  summarize(
    mean_time_margin = mean(time_margin, na.rm = TRUE),
    decade_end = decade_start + years(10) - days(1)
  ) %>% 
  unique()

tdf_decade_spread <- tdf %>% 
  group_by(decade_start) %>% 
  summarize(
    max_time_margin = max(time_margin, na.rm = TRUE),
    min_time_margin = min(time_margin, na.rm = TRUE)
  ) %>% 
  mutate(
    decade_end = decade_start + years(10) - days(1)
  )

tdf_label <- tdf_decade %>% 
  mutate(
    label_subst = substr(year(decade_start), 3, 4),
    label = paste0("'", label_subst)
  )

tdf_key <- tibble(
  x1 = ymd('1975-01-01'),
  x2 = ymd('1985-01-01'),
  y1 = 2,
  y2 = 2.75,
  label = c("Decade largest margin", "Decade smallest margin", "Decade Average margin")
)


# Plotting data
tdf_p1 <- tdf_decade %>% 
  ggplot() + 
  geom_rect(
    data = tdf_decade_spread,
    aes(xmin = decade_start, xmax = decade_end,
        ymin = min_time_margin, ymax = max_time_margin),
    fill = "#05f2cf33"
  ) +
  geom_segment(aes(x = decade_start, xend = decade_end,
                   y = mean_time_margin, yend = mean_time_margin),
               size = 2, color = "#05f2cfbb") +
  geom_point(data = tdf, aes(x = start_date, y = time_margin)) + 
  geom_text(data = tdf_label, aes(x = decade_start + years(5), 
                                  y = -.15, 
                                  label = label),
            family = "Roboto Lt") +
  geom_hline(yintercept = 0, color = grey(0.9)) 

# Plotting key
tdf_p2 <- tdf_p1 +
  geom_rect(data = tdf_key, 
            aes(xmin = x1, xmax = x2,
                ymin = y1, ymax = y2),
            fill = "#05f2cf18") +
  geom_segment(data = tdf_key,
               aes(x = x1, xend = x2,
                   y = 2.45, yend = 2.45),
               color = "#05f2cfbb", size = 2) +
  geom_segment(data = tdf_key,
               aes(x = x2 + months(6), xend = x2 + years(5),
                   y = c(2.75, 2, 2.45),
                   yend = c(2.75, 2, 2.45)),
               color = grey(0.8)
               ) +
  geom_text(data = tdf_key,
            aes(x = x2 + years(5) + months(6),
                y = c(2.75, 2, 2.45),
                label = label),
            hjust = 0, family = 'Roboto Lt', size = 4, color = grey(0.4))

# Finishing touches
tdf_final <- tdf_p2 + 
  labs(
    title = "Tight Margins in the Tour de France",
    subtitle = "This graphic explores the margins by which Tour de France winners have\nwon the General Classification (awarded to the fastest overall finishing\ntime). As technology and training have improved, GC winners have eked\nout a victory by tighter and tigher margins.",
    caption = "Source: {tdf} package  |  Visualization: @charliegallaghr"
  ) + 
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "10 years", expand = c(0,0)
  ) +
  scale_y_continuous(name = "Hours") +
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto Lt"),
    plot.background = element_rect(color = NA, fill = "#faffff"),
    plot.title = element_text(
      family = "Times New Roman", size = 30, margin = margin(0,0,0,0)
    ),
    plot.subtitle = element_text(
      family = "Roboto Lt", size = 12, margin = margin(5,0,10,0)
    ),
    plot.caption = element_text(
      size = 10, color = grey(0.5), hjust = 0.9
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

svg(filename = "tour.svg", width = 10, height = 7)
tdf_final
dev.off()