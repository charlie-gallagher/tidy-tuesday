# NFL attendance
library(tidyverse)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

nfl <- dplyr::left_join(attendance, standings, by = c("year", "team_name", "team"))

save(nfl, games, file = 'data/nfl.RData')


#=========================================================
library(tidyverse)
library(skimr)
library(rvest)
library(stringr)
library(sf)
load('data/nfl.RData')


skimr::skim(nfl)
skimr::skim(games)


games %>%
  count(away_team) %>%
  print.data.frame()
  # There is basically no data about the LA Rams and the Miami Dolphins

# Idea for a spatial analysis: How is a team's location related to its
# attendance? I'll need the coordinates of the stadiums, which means
# getting a list of stadium addresses. 

# Getting stadium data
stadiums <- readr::read_csv(file = 'data/stadiums.csv')

# This presents a challenge. Stadiums may change over time, which means
# attendance will change over time. Probably I should convert this to 
# a sort of panel in which there is one observation for every year-team 
# combination. This is a good challenge. First, some general cleaning.
stadiums <- stadiums %>%
  filter(To > 1999)
names(stadiums) <- c('stadium', 'from','to','games','city','state','teams')


# Split up columns and pivot
stadiums <- stadiums %>%
  separate(col = teams, sep = "-", into = c('first','second','third','fourth')) %>%
  pivot_longer(c(first, second, third, fourth)) %>%
  filter(!is.na(value)) %>%
  select(stadium, from, to, city, state, value)

stadiums <- stadiums %>%
  mutate(
    nyears = if_else(from < 2000, false = to - from, 
                     true = 19)
  )

reps <- rep(seq_along(stadiums$value), stadiums$nyears)
stadiums <- stadiums[reps,]

# Generate year variable
stadiums <- stadiums %>%
  group_by(value) %>%
  mutate(year = if_else(from < 2000,
                        true = row_number() + 1999,
                        false = row_number() + from)) %>%
  select(stadium, city, state, value, year)

names(stadiums) <- c('stadium', 'city', 'state', 'team', 'year')


# Some stadiums have both types of information but are disjoint because the 
# name changed. So, I merged based on team name instead, keeping both 
# stadium names. The more recent names are the ones I'm keeping, while
# the older names will help me merge with the other data. 
stadiums_capacity <- readr::read_delim(file = 'data/stadiums_capacity.csv', 
                                       delim = '\t')


stadium_names <- unique(stadiums[,c("stadium","team")])

stadium_names <- stadium_names %>%
  mutate(
    wiki_url = gsub(" ","_", stadium),
    wiki_url = paste0("https://en.wikipedia.org/wiki/", wiki_url)
  )


stadium_names$coordinates <- sapply(stadium_names$wiki_url, function(x) {
  webpage <- read_html(x)
  coord_html <- html_nodes(webpage, '#coordinates span')
  coord <- html_text(coord_html)[10]
  return(coord)
})

stadium_names <- stadium_names %>%
  separate(coordinates, into = c('latitude', 'longitude'),
          sep = '; ') %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  select(stadium, team, latitude, longitude)


stadiums_full <- stadium_names %>%
  full_join(stadiums_capacity, by = c('team'))


save(games, nfl, stadiums_full, stadiums, file = 'data/nfl.RData')




#===========================
# Putting it all together
#===========================
# I have stadium data and team data, I just need to put it all together. 

# I need to summarize the nfl data to the year-team level
# Logical cleaning
nfl <- nfl %>%
  mutate(
    playoffs = if_else(playoffs == "Playoffs", true = TRUE, false = FALSE),
    sb_winner = if_else(sb_winner == "Won Superbowl", true = TRUE, false = FALSE)
  )

nfl_sum <- nfl %>%
  group_by(year, team_name) %>%
  summarize(wins = mean(wins, na.rm = TRUE),
            losses = mean(loss, na.rm = TRUE),
            mean_points_for = mean(points_for, na.rm =TRUE),
            mean_points_against = mean(points_against, na.rm = TRUE),
            mean_margin_of_victory = mean(margin_of_victory, na.rm = TRUE),
            mean_home = mean(home, na.rm = TRUE),
            mean_away = mean(away, na.rm = TRUE),
            sb_winner = any(sb_winner, na.rm = TRUE),
            playoffs = any(playoffs, na.rm = TRUE)
            )

# In order to join, I need to remove the city from the team name in the
# stadiums dataset. Ach! that's not easy. That's really not easy. Do any teams
# have two names? Whew, none too bad then. 


stadiums_full <- stadiums_full %>%
  mutate(
    team_name = sapply(team, function(x) {
      word(x, -1)
    })
  )

# Number of home games
n_home <- games %>%
  group_by(year, home_team) %>%
  filter(week %in% as.character(1:17)) %>%
  summarize(n_home = n())


nfl_sum <- nfl_sum %>%
  left_join(stadiums_full, by = c('team_name')) %>%
  filter(!is.na(name), stadium == name) %>%
  left_join(n_home, by = c('year' = 'year','team' = 'home_team'))

save(games, nfl, nfl_sum, stadiums, stadiums_full, file = 'data/nfl.RData')


#===================
# Analysis
#===================
rm(list = ls())
load('data/nfl.RData')


# Adding capacity statistics
nfl_sum <- nfl_sum %>%
  mutate(
    mean_home_game = mean_home / n_home, 
    cap_ratio = 100 * ((mean_home / n_home) / capacity)
  )




nfl_sum %>%
  select(year, team, capacity, n_home, mean_home, playoffs, sb_winner) %>%
  mutate(home = mean_home / n_home,
         capacity_level = 100 * (home / capacity)) %>%
  group_by(year, playoffs) %>%
  summarize(capacity_level = mean(capacity_level, na.rm =TRUE),
            capacity = mean(capacity, na.rm = TRUE),
            home = mean(home, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = year, y = home, group = playoffs, 
                color = playoffs)) + 
  geom_text(aes(x = year, y = home, label = as.character(round(home)))) + 
  scale_color_brewer(type = 'qual', palette = 2) + 
  theme_light() 


nfl_sum %>%
  select(year, team, capacity, n_home, mean_home, playoffs, sb_winner) %>%
  mutate(home = mean_home / n_home,
         capacity_level = 100 * (home / capacity)) %>%
  group_by(year, sb_winner) %>%
  summarize(capacity_level = mean(capacity_level, na.rm =TRUE),
            capacity = mean(capacity, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = year, y = capacity_level, group = sb_winner, 
                color = sb_winner)) + 
  scale_color_brewer(type = 'qual', palette = 2) + 
  theme_light() 





# Ranking stadiums by size
stadiums_full %>%
  select(stadium, capacity, team) %>%
  group_by(stadium) %>%
  # Fixing doubled rows (same stadium, different teams)
  mutate(N = row_number()) %>%
  filter(N == 1, capacity > 0, !is.na(capacity)) %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(stadium, capacity, .desc = TRUE), y = capacity), fill = grey(0.8), color = 'black') + 
  geom_text(aes(x = stadium, y = capacity, label = stadium),
            angle = 90, hjust = 1.05, color = 'black') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x = element_blank())

# Do larger stadiums fill up less frequently? 
# What's a good measurement for this? Look at a scatter of size on x and 
# percent filled on y. 

nfl_sum_model <- nfl_sum %>%
  select(year, team, capacity, n_home, mean_home) %>%
  mutate(
    mean_home_game = mean_home / n_home, 
    cap_ratio = 100 * ((mean_home / n_home) / capacity)
    )

ggplot(nfl_sum_model, aes(x = capacity, y = cap_ratio)) +
  geom_point() + 
  geom_smooth()


#=====================================
# Combining with map data on CSAs ----
#=====================================
load('data/nfl.RData')
load('data/csa.RData')

# First, I need to convert nfl_sum to an sf-type dataset. This is alway 
# kinda a pain.
nfl_sf <- nfl_sum %>%
  ungroup() %>%
  filter(year == 2017) %>%
  mutate(
    mean_home_game = mean_home / n_home, 
    cap_ratio = 100 * ((mean_home / n_home) / capacity)
  ) %>%
  select(year, team, stadium, capacity, n_home, playoffs, sb_winner, 
         cap_ratio, mean_home_game,
         latitude, longitude)

# Convert x,y pairs to a list of numerics of length 2. Split by team and 
# recombine as a list of numeric pairs.
longlat <- nfl_sf %>%
  select(team, longitude,latitude)

longlat <- split(longlat[,2:3], list(longlat$team))
  # WATCH OUT this is in alphabetical order now. 
longlat <- st_sfc ( lapply(longlat, function(x) st_point(as.numeric(x))) )

# Change order of nfl_sf to match longlat
nfl_sf <- nfl_sf[order(nfl_sf$team),]

# Create sf object. 
nfl_sf <- st_sf(data.frame(nfl_sf, geometry = longlat),
                crs = st_crs(csa))

# Map of stadiums and CSAs ----------------------------------
par(mfrow = c(1,1), mar = c(0,0,0,0))
plot(st_geometry(state), reset = FALSE)
plot(st_geometry(nfl_sf), add = TRUE)
plot(st_geometry(csa), col = grey(0.6, alpha = 0.3), border = NA, add = TRUE)




# Now, we've got the stadium shapefile, let's get the name of the CSA that
# each stadium (well almost each stadium) is in. 
nfl_sf$csa <- csa$CSAFP[as.numeric(st_within(nfl_sf, csa))]
nfl_sf$csa_pop <- csa$pop[as.numeric(st_within(nfl_sf, csa))]

par(mfrow = c(1,1), mar = c(0,0,0,0))
plot(st_geometry(state), reset = FALSE)
plot(nfl_sf["csa_pop"], add = TRUE, pch = 20)
plot(st_geometry(csa), col = grey(0.6, alpha = 0.3), border = NA, add = TRUE)

save(nfl_sf, state, csa, file = 'data/nfl_sf.RData')




# Attendance as a ratio of CSA size -----------------------
load('data/nfl_sf.RData')
nfl_sf <- nfl_sf %>%
  mutate(
    att_to_csa = (mean_home_game / csa_pop)
  )

pdf(file = 'pdf/csa.pdf', width = 6, height = 5.5)
nfl_sf %>%
  select(team, mean_home_game, att_to_csa) %>%
  ggplot() + 
  geom_col(aes(x = fct_reorder(team, att_to_csa, .desc = TRUE), y = att_to_csa)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_text(data = filter(nfl_sf, team != "Green Bay Packers"),  
            aes(x = fct_reorder(team, att_to_csa, .desc = TRUE), 
                y = att_to_csa, label = team),
            angle = 90, hjust = 0, color = 'black' ) + 
  annotate(geom = 'text', x = 1.5, y = 0.2, label = 'Green Bay Packers', 
           hjust = 0) + 
  ylab("% of\n CSA in\nattendance") + 
  ggthemes::theme_tufte() + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 13),
    axis.title.y.left = element_blank()
  )
dev.off()

# Wow, the Greenbay Packers drew over 20% of their MSA on average in 2017. Let's look at a 
# map of the MSAs with teams, and then look at some basic statistics. 
# Population density, for example, and raw population, the presence of nearby
# MSAs with large populations, etc. 
par(mfrow = c(1,1), mar = c(0,0,0,0))
plot(st_geometry(state), reset = FALSE)
csa[as.numeric(st_within(nfl_sf, csa)),] %>%
  select(pop) %>%
  plot(add = TRUE, col = grey(0.7))
plot(st_geometry(nfl_sf), add = TRUE)
text(st_coordinates(nfl_sf), labels = nfl_sf[,"team", drop = TRUE], 
     cex = 0.7)
 
