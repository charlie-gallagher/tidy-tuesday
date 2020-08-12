# This week's data is about trees in San Francisco. I'm on the train so 
# I don't have any data yet, but I can start coming up with micro research
# questions at least. 

# Which counties have the highest concentration of trees? The greatest 
# diversity of trees?** When and where were most trees planted? Which 
# parks have the lowest density of trees (so they can plant more)? Which
# areas have seen growth or declines in trees? Where are the oldest trees? 
# Has the trend been towards biodiversity or have a few species been 
# chosen? Are some species hardier than others? Which last the longest? 
# Are there differences between trees in parks and trees along streets? 
# Are trees planted evenly in all decades, or did some decades choose parks
# while others chose street trees? Do tree policies line up with mayors or
# political party in power? Where are the tallest trees? Do street trees
# grow at the same rate as park trees? Is diversity different in parks 
# and along streets? I should move away form the parks v. streets thread now. 
library(sf)
library(tidyverse)
library(skimr)

#==================
# Import raw data
#==================
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

sf_shp <- st_read(dsn = "data",
                  layer = "sf_neighborhoods")


#==========================
# Convert tree data to sf
#==========================
# Need: 
#   * List column of coordinates as points (numeric vectors of length 2)
#   * As c(x,y)
sf_coord <- split(sf_trees[,12:11], sf_trees$tree_id)
sf_coord <- lapply(sf_coord, function(x) st_point(as.numeric(x)))
sf_coord <- st_sfc(sf_coord1)

sf_trees$coordinates <- sf_coord
sf_trees <- st_sf(sf_trees)

st_crs(sf_trees) <- st_crs(sf_shp)

save(sf_shp, sf_trees, file = 'data/sf_trees.RData')

#==================================
# Basic plots
#==================================
load('data/sf_trees.RData')

pdf('pdf/test_sf.pdf', height = 60, width = 60)
par(mar = c(0,0,0,0))
plot(st_geometry(sf_shp), )
plot(st_geometry(sf_trees), add = TRUE, pch = 20, cex = 0.05)
dev.off()

# It looks like every tree is one a street. The parks are barren, either
# because they don't have data collected about them or because they really have
# no trees. I'm not sure. 

# The first thing I need to do is to assign a neighborhood to each tree. This
# will use the st_whithin or something similar. 
sf_trees$neighborhood <- as.numeric(st_within(st_geometry(sf_trees), st_geometry(sf_shp), sparse = TRUE))
sf_trees$neighborhood_name <- sf_shp$nbrhood[sf_trees$neighborhood]

pdf('pdf/test_sf.pdf', height = 15, width = 15)
par(mar = rep(0,4))
plot(st_geometry(sf_shp), bg = NA, col = NA, border = "black", reset = FALSE,
     cex = 2)
plot(sf_trees[,"neighborhood_name"], pch = 20, cex = 0.1, add = TRUE)
dev.off()

# These images take a long time to render, so it's best to stick to 
# analytical challenges. Which neighborhood has the most trees? Which has the 
# greatest number of trees per square mile/foot/meter?
sf_tree_sum <- sf_trees %>%
  group_by(neighborhood_name) %>%
  summarize(sum_trees = n(),
            avg_height = median(dbh, na.rm = TRUE),
            n_species = length(unique(species)))
sf_tree_sum %>%
  arrange(desc(avg_height))
# Ooh and this made the coordinates into MULTIPOINT types. That's perfect for
# a neighborhood analysis.
par(mar = rep(2,4))
hist(sf_tree_sum$sum_trees)
sf_tree_sum %>%
  ggplot() + 
  geom_histogram(aes(avg_height))


# Stylized facts I would like to have: 
# Who is responsible for planting trees? What influences them? Are they
# influenced by politics or national environmental interests, or by developers? 
# Are developers the chief planters or is the municipality? Is tree height 
# a good indicator of tree age? 


# Some things I learned recently
# The count() function (a mix of group_by() and tally().)

# I want to count each species. First, I need to clean up the species data
species_split <- str_split(sf_trees$species, " :: ", simplify = TRUE)
# This returns either a list or matrix. The matrix probably converts more 
# easily to a data frame. 
sf_trees <- sf_trees %>%
  mutate(species_l = factor(species_split[,1]),
         species_c = species_split[,2])

species_count <- sf_trees %>%
  count(species_l) %>%
  mutate(species_l = fct_reorder(species_l, n))

species_count %>%
  filter(n > 1000, species_l != "::", species_l != "Tree(s) ::") %>%
  ggplot() + 
  geom_col(aes(x = species_l, y = n)) + 
  coord_flip()




# Okay, let's plot a map of the districts with the most popular tree in each
# district as the color. 
# Then I need to summarize by district, max(n)

# add_count() function works on the whole dataset (nice)
sf_trees <- sf_trees %>%
  add_count(species_l)

sf_trees %>%
  group_by(neighborhood_name) %>%
  summarize(max_n = max(n, na.rm = TRUE))





#-#_#_#_#_#_#_#_#_#_#__#_#_#
## IGNORE
#_#_#_#__##__#_#_#_#_#_#_#_#_

# Ignore
df <- tibble(
  a <- rnorm(10),
  b <- rnorm(10),
  c <- rnorm(10),
  d <- rnorm(10)
)
seq_along(df)


output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

# You can use double brackets for assignment in a for loop? 
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[i] <- median(df[[i]])      # 3. body
}
output


# Compute the mean of every column in mtcars
data(mtcars)
head(mtcars)
skim(mtcars)    # All columns are numeric

output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}

# Determine the type of column in nycflights13::flights
library(nycflights13)
data(flights)

output <- vector("character", ncol(flights))

for (i in seq_along(flights)) {
  output[i] <- typeof(flights[[i]])
}

# Compute the number of unique values in each column of iris
data(iris)
output <- vector('double', ncol(iris))

for (i in seq_along(iris)) {
  output[i] <- length(unique(iris[[i]]))
}

# Generate 10 random normals from distributions with means 
# of -10, 0, 10, and 100.
vec <- c(-10, 0, 10, 100)
output <- list(
  a <- vector('double', 10),
  b <- vector('double', 10),
  c <- vector('double', 10),
  d <- vector('double', 10)
)

for (i in seq_along(output)) {
  output[[i]] <- rnorm(10, mean = vec[i], sd = 1)
}

# Eliminate the for loops in each of the following loops by using an existing
# function.
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out <- ""
out <- glue::glue_collapse(letters)    # Creates a 'glue' string
out <- paste0(letters, collapse = "")  # Creates a normal string. I always 
                                       # forget about the collapse option.


x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
# This just calculates the standard deviation
x <- sample(100)
sd <- sd(x)




x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
# Adds the current term to the previous term, as in the Fibonacci sequence
# There's probably a time series method for this. I don't know what to 
# call it, though. 
x <- runif(100)
out <- cumsum(x)


