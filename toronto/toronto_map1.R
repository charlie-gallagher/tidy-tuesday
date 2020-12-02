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
  ggmap::mutate_geocode(address) %>%  # Requires ./register_google.R file
                                      # See above.
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


# Circles -------------------
# Use centroids of neighborhoods instead of polygons
nbr_points <- to_nbr %>% 
  filter(!is.na(yr_occupancy)) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Generate random points above chart
tmp_df <- data.frame(
  longend = runif(36, min = -79.7, max = -79.1),
  latend = runif(36, min = 43.85, max = 44.1)
) %>% 
  arrange(desc(longend))

# Add points to neighborhood data in east-to-west order
nbr_points <- nbr_points %>% 
  arrange(desc(LONGITUDE)) %>% 
  cbind(tmp_df)

# Plot
ggplot() + 
  geom_sf(data = to_nbr, aes(fill = yr_occupancy)) + 
  geom_sigmoid(
    data = nbr_points,
    aes(x = LONGITUDE, xend = longend,
        y = LATITUDE, yend = latend,
        group = AREA_ID, color = yr_occupancy),
    smooth = 20,
    size = 1,
    alpha = 0.7,
    direction = 'y'
  ) + 
  geom_point(data = nbr_points, 
             aes(x = longend, y = latend, 
                 size = yr_occupancy, 
                 color = yr_occupancy)
  ) + 
  scale_size_continuous(range = c(1, 20)) + 
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_blank()
  )



