library(tidyverse)
library(sf)
library(rnaturalearth)
library(extrafont)

# Get data
turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

canada <- rnaturalearth::ne_countries(country = c("canada", "united states of america"), 
                                      scale = "medium") %>% 
  as("sf")

# Convert turbine to SF object
turbine <- st_as_sf(turbine, coords = c("longitude","latitude"),
                       crs = 4326)


# Transform maps to a better projection
# For projections, see: spatialreference.org
canada_t <- canada %>%              st_transform(3573)
turbine_t <- turbine %>%            st_transform(3573)


# Zoom: To get the proper view with the US partially cropped, I need
# to make my own window with coord_sf. These values more or less from 
# guessing and checking
x_lim = c(-3000000, 3500000)
y_lim = c(-4950000, -1900000)



# Plot
distance <- st_distance(turbine_t)



t <- turbine_t %>% 
  mutate(
    n_within_50000m = apply(distance, 2, function(x) sum(x < 50000) -1)
  ) %>% 
  arrange(n_within_50000m) %>% 
  ggplot() + 
  # Plotting things
  geom_sf(data = canada_t, 
          aes(geometry = geometry, fill = sovereignt), 
          color = '#888888ff',
          size = 0.35) + 
  geom_sf(aes(geometry = geometry, color = n_within_50000m),
          size = 4, shape = 19, alpha = 0.1) + 
  coord_sf(xlim = x_lim, ylim = y_lim) +  # Set bounding box
  labs(
    title = "Wind Turbines of Canada",
    subtitle = "Each dot is a wind turbine in Canada. The color of the dot\nrepresents the number of other wind turbines within thirty\nmiles of that turbine -- in other words, turbine density.",
    caption = "Source: Government of Canada & Natural Earth  |  Visualization: @charliegallaghr"
) + 
  # Modifications
  scale_color_gradient(name = "Turbines within 30 miles", 
                       low = '#122e26', high = '#04c92c',
                       guide = 'legend') + 
  scale_fill_manual(values = c("#9a9a9a33", 'black')) +
  guides(
    color = guide_legend(override.aes = list('shape' = 19, 'alpha' = 1)),
    fill = FALSE
  ) +
  # Theme
  theme(
    plot.background = element_rect(fill = 'black'),
    plot.title = element_text(family = 'Abril Fatface', 
                              size = 30, 
                              color = '#ffffff',
                              margin = margin(10,0,0,0)),
    plot.subtitle = element_text(size = 14, 
                                 color = '#cccccc',
                                 margin = margin(0, 0, 10, 0)),
    panel.background = element_rect(fill = 'black'),
    panel.grid.major = element_line(color = '#adadad55'),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill = '#4a4a4a33'),
    legend.key = element_rect(fill = NA),
    text = element_text(color = '#8a8a8a', 
                        family = 'Roboto Lt'),
  )

svg(filename = 'turbines.svg', width = 11, height = 6.5)
t
dev.off()

