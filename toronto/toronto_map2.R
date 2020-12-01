library(ggplot2)
library(dplyr)
library(magrittr)
library(ggmap)            # Google geocode API
library(sf)               # GIS data structures
library(opendatatoronto)  # For Toronto street map data
library(ggbump)           # geom_sigmoid
source("./register_google.R")  # For Google geocode API, you need 
# to create your own. See below.

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

# Merge in address coordinates and make sf
to_shelters <- to_shelters %>% 
  left_join(st_drop_geometry(shelters_addr), by = "address") %>%
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326,
               remove = FALSE)



