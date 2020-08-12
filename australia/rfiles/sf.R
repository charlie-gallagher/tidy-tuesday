# Source: Pebesma (2018). Simple Features in R.
# See notes at work on small white notepad for more theory. 

library(tidyverse)
library(sf)
library(sp)
library(rgdal)


(pt <- st_point(c(0,1)))
(pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
st_as_sfc("POINT(0 1)") # returns sfc


(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
# Returns list of length 1 with a 5x2 matrix. 

# All functions begin with "st_", which stands for "spacetime".


# Multiple graphs based on attributes
nc <- read_sf(system.file("gpkg/nc.gpkg", package="sf"))
plot(nc[, c(9,5)])

# Single graph; gets legend. 
plot(nc[, 9], key.pos = 1, axes = TRUE, graticule = TRUE)

# Only geometry, no legend or title
plot(st_geometry(nc))

# ggplot version
(nc2 <- nc %>% st_transform(32119) %>% select(SID74, SID79, geom) %>%
  gather(VAR, SID, -geom))
ggplot() + geom_sf(data = nc2, aes(fill = SID)) + facet_wrap( ~ VAR, ncol = 1)


# Starting the vignettes
nc <- st_read(system.file("shape/nc.shp", package="sf"))
print(nc[9:15], n = 3)

methods(class = "sf")

(nc_geom <- st_geometry(nc))


# plotting geometry
par(mar = c(0,0,1,0))
plot(nc[1], reset = FALSE)
plot(nc[1,1], col = "grey", add = TRUE)


# Which polygons have more than one geometry? 
which(sapply(nc_geom, length) >1)
# sapply returns vector of values; >1 returns a logical the same length. Which()
# converts the list of logicals to a short list of index numbers. 

attributes(nc)      # NOTE: sf_column attribute tells which is geometry list
attributes(nc_geom) # 

# Multiple types of geometries go in a GEOMETRYSET
(mix <- st_sfc(st_geometrycollection(list(st_point(1:2))),
  st_geometrycollection(list(st_linestring(matrix(1:4,2))))))


(mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2))))


# Examples of creator functions (rarely used in practice, good for tests 
# and examples)
x <- st_point(c(1,2)) # numeric vector (usually length 2) forms a point
x <- st_point(c(1,2,3))
x <- st_point(c(1,2,3), "XYM")
x <- st_point(c(12,3,4))
st_zm(x, drop = TRUE, what = "ZM")

# A POINT is a numeric vector
# A set of points (LINESTRING, POLYGON, etc.) is a MATRIX
# Any other set is a LIST

# Making a polygon with a hole
p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
(pol <- st_polygon(list(p1, p2)))
plot(pol, col = "grey")



#=======================
# Reading and Writing
#=======================

setwd("C:/Users/Charlie's Surface/Documents/R/Shapefiles")
us <- st_read("cb_2018_us_state_500k/cb_2018_us_state_500k.shp", 
              stringsAsFactors = FALSE)
plot(us[-c(14,28,38,39,43,45,46),4])

# NOTE: st_read() is substantially faster than rgdal::readOGR()

# Question: How do I draw just the map? No colors, no variable associated, 
# just the map. 
# Answer: Use st_geometry to isolate the geometry. 
territories <- c(14,28,38,39,43,45,46)
plot(st_geometry(us[-territories,4]))


# Geometrical operations
# All start with st_*
# is_valid, is_simple, distance, relate

# Functions that return a sparse or logical matrix:
# intersects, disjoint, touches, crosses, within, contains, overlaps, equals, 
#       covers, covered by, equals_exact, is_within_distance

nc <- st_read(system.file("shape/nc.shp", package = "sf"))
st_intersects(nc[1:5,], nc[1:4,])  # Returns sparse matrix
st_intersects(nc[1:5,], nc[1:4,], sparse = FALSE)  # Returns logical matrix
plot(st_geometry(nc[1:5,]))

# Functions that return new geometries
# buffer, boundary, convexhull, union_cascaded, simplify, triangulate, 
#       polygonize, centroid, segmentize, union
par(mar = rep(0,4))
plot(st_union(nc))
plot(st_union(us[-territories,]))
  # NOTE: No attributes are kept (How cculd they be kept? Consider summarize() 
  #       method.)
plot(summarize(nc, max(AREA, na.rm = TRUE)))
  # Oooohh the summary method works nicely. 
summary(summarize(nc, max(AREA, na.rm = TRUE)))

# Let's see if this works for summarizing at the county level from the 
# tract level. 
setwd("~/R")
load("shapefiles/sepa.RData")

plot(pa.tracts)
plot(pa.counties)

pa_tracts <- st_as_sf(pa.tracts)
pa_counties <- st_as_sf(pa.counties)

pa_tracts <- pa_tracts %>%
  mutate(
    countyfips = paste0(STATEFP, COUNTYFP),
    land = as.numeric(ALAND)
  )

plot(pa_tracts[,"land"])

# Interesting, you say, but what about the counties? In this case, summarize
# is more useful than union. 
pa_se_county <- summarize(group_by(pa_tracts, countyfips))
plot(pa_se_county)

par(mar = c(0,0,0,0))
plot(st_geometry(pa_tracts), lwd = 0.5)
plot(st_geometry(pa_se_county), lwd = 2, add = TRUE)

# Can I color the counties in opaque colors? Only in ggplot. 


# Functions and transformations--------------------------
# You can convert all geometries to a single type using the st_cast() function. 
# Use this to simplify or cast to a specific type. Works for both list columns
# and single geometries. 

st_crs(pa_tracts) # Get CRS
  # EPSG might be useful later: 4269
  # st_transform(pa_tracts, 4269) Something like this maybe. 

st_crs(pa_tracts) <- 4269    # Use this method to change the CRS
s1 %>% st_set_crs(4269)      # Or you can use this method (pipe friendly)

# To change the actual projection of the coordinates, you use st_transform()


# Geometrial operations---------------------------------
# Geometrical operations take both 'sf' and 'sfc' objects, but only return
# 'sfc' objects. There are two types: st_op(x) and st_op2(x,y). When 
# st_op2(x) is called, it repeats x, so st_op2(x,x). 

# Examples
b0 <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 <- b0 + 2
b2 <- b0 + c(-0.2,2)
x <- st_sfc(b0, b1, b2)
a0 <- b0 * 0.8
a1 <- a0 * 0.5 + c(2, 0.7)
a2 <- a0 + 1
a3 <- b0 * 0.5 + c(2, -0.5)
y <- st_sfc(a0,a1,a2,a3)
plot(x, border = 'red')
plot(y, border = 'green', add = TRUE)

st_area(x)
st_area(st_sfc(st_point(c(0,0))))
st_length(st_sfc(st_linestring(rbind(c(0,0), c(1,1), c(1,2))),
                 st_linestring(rbind(c(0,0), c(1,0)))))

st_distance(x,y)
st_relate(x,y)
st_intersects(x,y, sparse = FALSE)


# Returning a geometry
plot(st_intersection(x,y), col = "blue", add = TRUE)
plot(st_union(x), col = "black", add = TRUE)

u <- st_union(x)
par(mfrow = c(1,2), mar = rep(0,4))
plot(st_buffer(u, 0.2))
plot(u, border = 'red', add = TRUE)
plot(st_buffer(u, 0.2), border = 'grey')
plot(u, border = 'red', add = TRUE)
plot(st_buffer(u, -0.2), add = TRUE)

plot(st_boundary(x))

par(mfrow = c(1,2))
plot(st_convex_hull(x))
plot(st_convex_hull(u))

par(mfrow = c(1,1))
plot(x)
plot(st_centroid(x), add = TRUE, col = 'red')
plot(x)
plot(st_centroid(u), add = TRUE, col = 'red')

par(mfrow = c(2,2), mar = c(0,0,1,0))
plot(x, col = '#ff333388')
plot(y, add = TRUE, col = '#33ff3388')
plot(x, border = 'grey')
plot(st_difference(st_union(x), st_union(y)), col = 'lightblue', add = TRUE)
plot(x, border = 'grey')
plot(st_difference(st_union(y), st_union(x)), col = 'lightblue', add = TRUE)
plot(x, border = 'grey')
plot(st_sym_difference(st_union(y), st_union(x)), col = 'lightblue', add = TRUE)




# ggplot2 plotting method: geom_sf. 
library(sf)
library(ggplot2)
library(maps)
usa <- st_as_sf(map('usa', plot = FALSE, fill = TRUE))
laea <- st_crs("+proj=laea +lat_0=30 +lon_0=-95")
usa <- st_transform(usa, laea)
ggplot() + geom_sf(data = usa)



#===============================
# Manipulating simple features
#===============================
#   * Aggregation
#   * Summarizing
#   * Joining based on geometry


# The geometry column is 'sticky' in that it is always attached to a subset. 
nc <- st_read(system.file("shape/nc.shp", package = "sf"))
nc[1,]
nc[,"BIR79"]     # geometry still attached
nc[,"BIR79", drop = TRUE]   # Returns ONLY BIR79; no geometry

# Subset using another geometry as row selector
Ashe <- nc[nc$NAME == "Ashe",]
nc[Ashe,]            # Uses st_intersects to choose which geometries to return

plot(st_geometry(nc[Ashe,]))
plot(st_geometry(Ashe), add = TRUE, col = 'red')


# You can choose which operation you use to subset with the op= argument
nc[Ashe, op = st_touches]

plot(st_geometry(nc[Ashe, op = st_touches]))        # Ashe missing here
plot(st_geometry(Ashe), add = TRUE, col = 'red')    # Here I bring it back

# The same operation in dplyr
nc %>% filter(lengths(st_touches(., Ashe)) > 0)

st_touches(nc, Ashe)

# AGGREGATING OR SUMMARIZING FEATURE SETS----------------------------------
# Compare fraction of SID of the counties that intersect with Ashe to the 
# remaining ones. 
a <- aggregate(nc[, c("SID74", "BIR74")], 
               list(Ashe_nb = lengths(st_intersects(nc, Ashe)) > 0), sum)
  # What does this do? Aggregate function: 
# aggregate(x, by, FUN, ..., do_union = TRUE,
#           simplify = TRUE, join = st_intersects)
# So, my guess is that this performs a union on those counties that surround
# Ashe county, summing the number of deaths and births. 
#   * subset for only SID74 and BIR74
#   * "lengths(st_intersects(nc, Ashe))>0" returns a logical vector of those
#     that intersect with Ashe. st_intersects returns a sparse matrix with N
#     rows (N = number of observations); lengths()>0 gives a logical telling
#     which rows have a row of the sparse matrix that isn't empty, of length N.
#   * Then why put it into a list format? This is a requirement of 
#     stats::aggregate.data.frame(). 

(a <- a %>% mutate(frac74 = SID74 / BIR74) %>% select(frac74))
plot(a[2], col = c(grey(0.8), grey(0.5)), reset = FALSE)
plot(st_geometry(Ashe), border = "#ff8888", add = TRUE, lwd = 2)


# Merges work as expected
# You can also merge via geometry using st_join()
x <- st_sf(a = 1:3, geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
y <- st_buffer(x, 0.1)
x <- x[1:2,]
y <- y[2:3,]
plot(st_geometry(x), xlim = c(0.5,3.5))
plot(st_geometry(y), add = TRUE)

# st_join always uses LEFT_JOIN. The geometry retained is that of the 
# first argument.
plot(st_join(x,y))
plot(st_join(y,x))

# You can also choose your join method
st_join(x,y, join = st_covers) # No matching y records: points don't cover circles
st_join(y, x, join = st_covers)


# Aggregate
#   See, I learned to use aggregate with formulas, which seems much more 
#   convenient than this list business. Oh well. 
df <- data.frame(
  x = c(1:10),
  y = rep(c(1:2),5)
)
aggregate(df, list(y = df$y), sum)

aggregate(list(aveWind = airquality$Wind), list(Month = airquality$Month),
          median)




#===========================
# Plotting simple features
#===========================
nc <- st_read(system.file('shape/nc.shp', package = 'sf'))
plot(nc["AREA"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), 
     key.length = 1.0)

nc$f <- cut(nc$AREA, 10)
plot(nc["f"], axes = TRUE, key.pos = 4, pal = sf.colors(10), key.width = lcm(4.5))

# Class intervals
plot(nc["AREA"], breaks = seq(0,0.25, by = 0.05))
plot(nc["AREA"], breaks = "jenks")


# Graticules
library(maps)
usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))
laea = st_crs('+proj=laea +lat_0=30 +lon_0=-95')
usa <- st_transform(usa, laea)
g <- st_graticule(usa)
plot(st_geometry(g), axes = TRUE)
plot(st_geometry(usa), graticule = TRUE, axes = TRUE)

g <- st_graticule(usa, long = seq(-130,-65,5))
plot(usa, graticule = g, key.pos = NULL, axes = TRUE,
     xlim = st_bbox(usa)[c(1,3)], ylim = st_bbox(usa)[c(2,4)],
     xaxs = "i", yaxs = "i")
  # This doesn't look great even still. But it's more customizable. 

# ggplot2
ggplot(data = usa) + geom_sf()
ggplot(nc) + geom_sf(aes(fill = BIR74)) +
  scale_y_continuous(breaks = 34:36)

#     facet
nc2 <- nc %>%
  select(SID74, SID79, geometry) %>%
  gather(VAR, SID, -geometry) %>%
  ggplot() + 
  geom_sf(aes(fill = SID)) + 
  facet_wrap(vars(VAR, ncol = 1)) + 
  scale_y_continuous(breaks = 34:36)
nc2

library(mapview)
mapview(nc["BIR74"], col.regions = sf.colors(10))



# Just as I finish learning the basics of the 'sf' package, I learn that 
# there is another package called 'tmap' for plotting 'sf' objects. 
# The list of packages is never going to end. 

# tmap specializes in 'thematic maps' like choropleths. 

library(tmap)
qtm(nc)       # qtm stands for "quick tmap"

data("World")
tm_shape(World)        # tm_shape() specifies a shape object, nothing is plotted
                       # I guess this is like ggplot(); initializes the object.
## Error: no layer elements defined after tm_shape


tm_shape(World) +
  tm_polygons("HPI")

# There is some stuff to learn. Here is how you plot layers:
data(World, metro, rivers, land)

tmap_mode("plot")
## tmap mode set to plotting
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)


# see https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html
# for more information. Also, see the bookmarked page on tidycensus and tmap.
