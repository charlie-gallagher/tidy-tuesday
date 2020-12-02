library(ggplot2)
library(dplyr)
library(magrittr)
library(ggmap)            # Google geocode API
library(sf)               # GIS data structures
library(opendatatoronto)  # For Toronto street map data
library(ggbump)           # geom_sigmoid
source("./register_google.R")  # For Google geocode API; you need 
# to create your own file. See below. 

# Google API file format -----------
# *********************
# register_google.R
# *********************
# ggmap::register_google(key = "your_api_key")



# Get data ------------
# Shelter data from TidyTuesday
to_shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')%>% 
  mutate(
    address = paste0(shelter_address, ", ", shelter_city, 
                     ", ", shelter_province,
                     " ", shelter_postal_code, ", Canada")
  )

# Neighborhood plotting data from {opendatatoronto}
to_nbr <- list_package_resources("https://open.toronto.ca/dataset/neighbourhoods/") %>%
  get_resource()

# Address coordinates (Geocoding with Google Maps)
shelters_addr <- to_shelters %>% 
  select(address) %>%
  unique() %>% 
  ggmap::mutate_geocode(address) %>% 
  st_as_sf(
    coords = c("lon","lat"),
    crs = 4326,
    remove = FALSE
  )


# Get neighborhoods of addresses
nb_id <- st_within(shelters_addr, to_nbr) %>% unlist()
shelters_addr <- cbind(
  shelters_addr, 
  st_drop_geometry(to_nbr)[nb_id, "AREA_SHORT_CODE"]
)
rm(nb_id)


# Make datasets ----------
# Merge in address coordinates and make sf
to_shelters <- to_shelters %>% 
  left_join(st_drop_geometry(shelters_addr), by = "address") %>%
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326,
               remove = FALSE)

# Summarize shelters by neighborhood for neighborhood occupancy
shelter_neighborhood <- to_shelters %>% 
  st_drop_geometry() %>% 
  filter(lubridate::year(occupancy_date) == 2019) %>% 
  group_by(AREA_SHORT_CODE) %>% 
  summarize(
    yr_capacity = sum(capacity, na.rm = TRUE),
    yr_occupancy = sum(occupancy, na.rm = TRUE)
  )

to_nbr <- to_nbr %>% 
  left_join(shelter_neighborhood, by = "AREA_SHORT_CODE")


# Bar chart ------------
baseline <- 43.88  # bar chart baseline latitude

# Use centroids of neighborhoods instead of polygons
nbr_points <- to_nbr %>% 
  filter(!is.na(yr_occupancy)) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Generate bar positions (east-to-west order)
tmp_df <- data.frame(
  longend = seq(-79.65, -79.1, length.out = 36),
  latend = baseline
) %>% 
  arrange(desc(longend))

# Add bar positions in east-to-west order
nbr_points <- nbr_points %>% 
  arrange(desc(LONGITUDE)) %>% 
  cbind(tmp_df) %>% 
  mutate(
    latend = latend + (yr_occupancy/5000000),
    yr_occupancy_txt = paste0(substr(yr_occupancy, 1, 2), " k")
  )

# Plot
ggplot() + 
  geom_sf(data = to_nbr, aes(fill = yr_occupancy)) + 
  geom_sigmoid(
    data = nbr_points,
    aes(x = LONGITUDE, xend = longend,
        y = LATITUDE, yend = baseline - 0.005,
        group = AREA_ID, color = yr_occupancy),
    smooth = 6,
    size = 1,
    direction = 'y'
  ) + 
  geom_segment(
    data = nbr_points,
    aes(x = longend, xend = longend,
        y = baseline, yend = latend,
        color = yr_occupancy),
    size = 2
  ) +
  geom_text(
    data = nbr_points,
    aes(x = longend, y = latend + 0.003, label = yr_occupancy_txt),
    size = 2.8, angle = 90, hjust = 0
  ) +
  scale_size_continuous(range = c(1, 20)) + 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_blank()
  )


