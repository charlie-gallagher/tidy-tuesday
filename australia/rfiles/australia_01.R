# australia_01.R
#----------------------------------
# Getting the fire data
#----------------------------------
library(sp)
library(rgdal)
library(tidyverse)
library(sf)

#=======================
# Final Version (sp)
#=======================
# Data
load("data/aus_map.RData")

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"
fires <- st_read(url)

aus_shp <- readOGR("data/ne_states_australia", 
                   layer = "ne_50m_admin_1_states_provinces")
aus_shp <- aus_shp[which(aus_shp$geonunit=="Australia"),]

# sf is better...
aus_sf <- st_read("data/ne_states_australia", 
                         layer = "ne_50m_admin_1_states_provinces")

# Plotting
oldbbox <- aus_shp@bbox
newbbox <- rbind(c(140,155), c(-38,-29))
aus_shp@bbox <- newbbox

pdf(file = "pdf/australia_01.pdf", height = 8, width = 12)
par(mar = c(0,0,0,0) + 3)
plot(aus_shp, axes = TRUE, col = "#f0ddc2", bg = "#f6f5ff")
text(x = coordinates(aus_shp)[c(6,9),1], y = coordinates(aus_shp)[c(6,9),2],
     labels = aus_shp@data$name[c(6,9)])
plot(fires[,3], add = TRUE)
plot(aus_spatial[1:10,], pch = 20, add = TRUE)
text(x = coordinates(aus_spatial)[1:10,1], y = coordinates(aus_spatial)[1:10,2],
     labels = aus_spatial@data$city[1:10], pos = 4)
dev.off()









# Experiments with 'fires'
plot(st_geometry(fires[1,]), col = NA)
plot(st_geometry(fires[,]), col = NA)

fire1 <- fires[1,]
plot(st_geometry(fire1))
fire1_geom <- st_geometry(fire1)
# How can I get only the points in this geometry collection? Maybe using 
# sapply() or something similar... Points are the only obejcts stored as 
# numeric vectors, so is.numeric should work. 
length(fire1_geom)          # length of 1
length(fire1_geom[[1]])     # length of 2

sapply(fire1_geom[[1]], FUN = st_geometry_type)
  # The first object is a point, the second is a geometry collection

sapply(fire1_geom[[1]][[2]], st_geometry_type)
sapply(fire1_geom[[1]][[2]], lengths)
  # 30 POLYGONS ranging in length from 8 to 422.

plot(st_cast(fire1_geom[[1]][[2]], to = "GEOMETRYCOLLECTION"))
