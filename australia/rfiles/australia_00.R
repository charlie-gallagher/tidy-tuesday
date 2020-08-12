# Tidy data scraping from Wikipedia
# 2020.01.09 (Jan. 9, 2020)

# Description----------------------------------------------------------------
# In this script, I scrape the Wikipedia table for populations of the largest
# cities in Australia. Then, I merge that data with coordinate data from
# geonames.org and convert the whole thing to a SpatialPointsDataFrame.
#----------------------------------------------------------------------------


library(rvest)
library(tidyverse)
library(lubridate)

url <- c("https://en.wikipedia.org/wiki/List_of_cities_in_Australia_by_population")
webpage <- read_html(url)


# Example processes
# table <- webpage %<%
#   html_nodes("table.vcard") %<%
#   html_table(header=F)
# table <- table[[1]]
# 
# #add the table to a dataframe
# dict <- as.data.frame(table)

# Another simple example of viewing the html of a webpage
# url %>%
#   read_html() %>%
#   html_node('body')



# Using an xpath to extract the data content
aus_cities <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)

names(aus_cities) <- c("rank","city","state","pop_2018",
               "pop_2011", "growth_pct","percent_pop")
aus_cities <- select(aus_cities, city, state, pop_2018)

aus_cities$pop_2018 <- gsub(",", "", aus_cities$pop_2018)
aus_cities$pop_2018 <- as.numeric(aus_cities$pop_2018)
str(aus_cities)

### SAVE ###
save(aus_cities, file = "data/aus_cities.RData")



# City coordinates
library(data.table)
aus_coord <- fread(file = "data/AU.txt", fill = TRUE)
aus_coord[grep("^Sydney$", x = aus_coord$V2)]

# Getting city coordinates for our list
x <- list()
for (i in 1:96) {
  x[[i]] <- grep(paste0("^",aus_cities[i,1], "$"), x = aus_coord$V2)
}

aus_coord <- aus_coord[unlist(x),]

# I think I can remove every one that has no population. First, name
# some columns I know for sure. 
names(aus_coord) <- c("id","city","city1","4","lat","long","uk1","uk2",
                      "country","uk3","uk4","uk5",
                      "uk6","uk7","population","uk8","uk9","region","date")

aus_coord <- aus_coord %>%
  select(city, lat, long, country, 
         population, region, date) %>%
  filter(population>0) %>%
  group_by(city)

aus_coord$date <- ymd(aus_coord$date)
aus_coord <- aus_coord %>%
  mutate(latest = date == max(date, na.rm = TRUE)) %>%
  filter(latest==TRUE)

aus_coord <- aus_coord[-c(35, 38, 48, 60, 65, 72, 81, 83),]
  # Had to do this last part observation by observation. 
  # In hindsight, the one with the greater population was always the right one. 


### SAVE ###
save(aus_coord, file = "data/aus_coord.RData")



# MERGE----------------------
load("data/aus_coord.RData")
load("data/aus_cities.RData")
aus <- left_join(aus_cities, aus_coord, by = "city")

# Missing 20 values. 
aus_coord <- fread(file = "data/AU.txt", fill = TRUE)
# This will get back some of them. 
aus_coord <- aus_coord[c(45341, 35744, 52748, 58386, 37965, 29937, 13658),
                       c(2,5,6,9,18)]
names(aus_coord) <- c("city","lat","long","country","region")

aus[c(6, 7, 8, 19, 28, 29, 31),"lat"] <- aus_coord$lat
aus[c(6, 7, 8, 19, 28, 29, 31),"long"] <- aus_coord$long
# What an ugly way to do this. 

aus <- select(aus, city, pop_2018, lat, long)

save(aus, file = "data/aus.RData")
#-----------------------------------------------------------------------
load("data/aus.Rdata")
library(sp)
aus_points <- SpatialPoints(coords = as.matrix(aus[!is.na(aus$lat),c(4,3)]))
# Well this seems to work well. 
aus_spatial <- SpatialPointsDataFrame(coords = aus_points,
                                      data = aus[!is.na(aus$lat),])
# Spatial object is done! It just needs a projection.


#--------------------------------------------------------------------------
library(rnaturalearth)
library(rnaturalearthdata)

aus_map <- ne_countries(country = "australia", scale = "medium")
plot(aus_map)

aus_crs <- proj4string(aus_map)
aus_spatial@proj4string <- CRS(aus_crs)

plot(aus_map)
plot(aus_spatial, add = TRUE)

save(aus_map, aus_spatial, file = "data/aus_map.RData")


#-----------------------------------------------------------------------------
# PLOTTING
#-----------------------------------------------------------------------------
aus_map_1 <- fortify(aus_map)

theme_set(theme_minimal())
nogrid <- theme(
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank()
)
ggplot(aus_map_1) + 
  geom_polygon(aes(x = long, y = lat, group = group)) + 
  coord_map() + 
  geom_point(data = aus, aes(x = long, y = lat, size = pop_2018),
             color = "red", alpha = 0.4) + 
  scale_size(range = c(1,15), breaks = c(1e+05, 1e+06, 5e+06)) + 
  labs(title = "Australia", subtitle = "90 largest cities") +
  nogrid

# Plan for tomorrow: 
#   * Get fire data
#   * Compare fires and locations of major cities
#   * Make it look nice