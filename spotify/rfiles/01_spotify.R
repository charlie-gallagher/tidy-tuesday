# Spotifyr and song features-----------------------------------------------

library(spotifyr)
library(tidyverse)
library(lubridate)

Sys.setenv(SPOTIFY_CLIENT_ID = "858e1824192649719bb6d38749a3b1ab")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "8e32b654a5e4440eb6b03caa78feb951")

access_token <- get_spotify_access_token()


beatles <- get_artist_audio_features('the beatles')

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5)


# The tidyTuesday dataset
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

table(spotify_songs$playlist_genre)
table(spotify_songs$playlist_subgenre)

# Relative popularity of each subgenre in each genre category
# Probably a stacked bar with dodging. 
ggplot(spotify_songs, aes(factor(playlist_genre), fill = playlist_subgenre)) + 
  geom_bar(position = position_dodge(), color = "black") + 
  geom_text(aes(x = playlist_genre, label = playlist_subgenre), 
            stat = "count", position = position_dodge(0.9), angle = 90,
            hjust = 1) +
  guides(fill = FALSE) +   # Use these to make it circular bar graph
  ylim(-3000, 2000) +      # Creates white space at center of circle
  coord_polar()
    # In this case, there aren't enough categories to make this interesting,
    # and this is DEFINITELY not made up for by the fact that this graph 
    # is useless for making comparisons. 
# For the labels, you need to calculate the label angle and adjustment: 
# # calculate the ANGLE of the labels
# number_of_bar <- nrow(label_data)
# angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar   
#          # I substract 0.5 because the letter must have the angle of the 
#          # center of the bars. Not extreme right(1) or extreme left (0).
# 
# # calculate the alignment of labels: right or left
# # If I am on the left part of the plot, my labels have currently an angle < -90
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
# # ----- ------------------------------------------- ---- #

# Instead of filling by type (where each bar is a different color) I want to 
# fill by rank. 
spotify_sum <- spotify_songs %>%
  group_by(playlist_genre, playlist_subgenre) %>%
  summarize(N = n()) %>%
  arrange(playlist_genre, desc(N)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

g <- spotify_sum %>%
  ggplot() + 
  geom_col(aes(x = playlist_genre, y = N, group = rank, fill = factor(rank)),
           position = "dodge", color = "black") + 
  geom_text(aes(x = playlist_genre, y = N-10, label = playlist_subgenre, 
                group = rank),
            position = position_dodge(width = 0.9), angle = 90, hjust = 1,
            vjust = 0.5)

g + 
  scale_fill_brewer(palette = 6, name = "Rank") +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = FALSE) + 
  coord_cartesian(ylim = c(0,1900)) + 
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank())



# Looking at densities
feature_names <- names(spotify_songs)[12:23]

spotify_songs %>%
  select(c('playlist_genre', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = playlist_genre), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Genre',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_brewer(type = 'qual', palette = 1)


# Function for choosing colors. '...' is an index of colors from kp_colors.
kp_cols <- function(...) {
  kp_colors <- c(purple = "#490B32",
                 red = "#9A031E",
                 orange = "#FB8B24",
                 dark_orange = "#E36414",
                 dark_blue = "#0F4C5C",
                 grey = "#66717E",
                 light_green = "#B3CBB9",
                 blue = "#5DA9E9"
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (kp_colors)
  
  kp_colors[cols]
}


spotify_songs %>%
  select(feature_names) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     col = colorRampPalette(colors = c(
                       kp_cols('red'), 
                       'white', 
                       kp_cols('dark_blue')))(200),
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Avenir')

# everyting with 'kp' is her own personal theme or function. 

# Corrplot is an interesting package. It's pretty flexible, and produces
# very nice results. 

spotify_songs %>%
  select(feature_names) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(type = 'upper', order = 'hclust', 
                     col = c("black","white"), bg = 'lightblue',
                     diag = FALSE)

# Size represents correlation strength; color represents direction.



# Lyrics? 
beatles <- get_discography('the beatles')
band <- get_discography('band of horses')

beatles <- beatles[108:139,]  %>%
  select(album_name, artist_name, artist_id, album_id, album_type, track_name, lyrics)

lyrics <- beatles$lyrics

# Right now I have a list of lists. This isn't convenient, and there's no 
# reason to retain this format as I don't care which song the lyrics came from.
# unlist() works when recursive = FALSE. 
lyrics <- unlist(lyrics, recursive = FALSE)

# I don't need all of the line numbers, so I can remove those
lyrics <- subset(lyrics, rep(c(F,T), 32))

# And now to clean and split
lyrics <- lapply(lyrics, function(y) gsub("\\(|\\)|\\,|\\|\\?|\\!'", "", y))
lyrics <- lapply(lyrics, tolower)
lyrics <- lapply(lyrics, function(x) gsub(" ", ",", x))
lyrics <- lapply(lyrics, function(x) unlist(str_split(x, ",")))
lyrics <- unlist(lyrics, use.names = FALSE)

lyrics_tab <- as.data.frame(table(factor(lyrics)))
names(lyrics_tab) <- c("lyric","count")

# To order the bars properly, I need to make a list of names in the right order
lyrics_tab <- lyrics_tab %>%
  arrange(desc(count)) %>%
  mutate(rank = row_number(),
         count = as.numeric(count)) %>%
  filter(rank <= 100)

lyrics_tab$lyric <- factor(lyrics_tab$lyric, 
                           levels = lyrics_tab$lyric[order(lyrics_tab$rank)])


lyrics_tab %>%
  ggplot() + 
  geom_col(aes(lyric, count, group = rank)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-100,301)) +
  coord_polar() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.ticks = element_blank()
  )

# Each point is at some degree, measured as an angle. That leaves a range of 
# zero to 365. The angle has to start with 0 = 90 degrees, so add 90 to 
# everything. I will need the number of bars and the rank of each bar. 
num_row <- nrow(lyrics_tab)
angle <- 90 - ((lyrics_tab$rank - 0.5) / num_row) * 360

# Some are greater than 360, so let's fix them. 
angle <- ifelse(angle<0, angle + 360, angle)

lyrics_tab <- cbind(lyrics_tab, angle)
rm(angle)

gg <- lyrics_tab %>%
  filter(rank<=100) %>%
  ggplot() + 
  geom_col(aes(lyric, count, group = rank, fill = -rank)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-100,306)) +
  coord_polar() + 
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(0,0,-150,0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
pdf(file = "pdf/beatles_lyrics.pdf", height = 14, width = 14)
gg + 
  geom_text(aes(lyric, count + 5, label = lyric, group = rank, angle = angle),
            hjust = 0, size = 2.5) + 
  guides(fill = FALSE) + 
  labs(title = "Popularity of Beatles Lyrics Words")
dev.off()











# Trying other bands
band <- get_discography('band of horses')
band <- band %>%
  select(artist_name, album_name, album_release_year,
         track_name, lyrics)
band$lyrics[1]
# NOTE: This is organized as a data.frame. That's fine, it functions the 
# same as a list. I'll keep only the lyrics, not the lines. And remove nulls.
band <- band[!sapply(band$lyrics, is.null),]

band$lyrics <- sapply(band$lyrics, function(x) select(x, lyric))

# So now I have a list of vectors instead of a list of lists. Nice. Easier to
# work with. Now the process is the same. 

band$lyrics <- lapply(band$lyrics, function(y) gsub("\\(|\\)|\\,|\\|?|\\!|\\'", "", y))
band$lyrics <- lapply(band$lyrics, tolower)
band$lyrics <- lapply(band$lyrics, function(x) gsub(" ", ",", x))
band$lyrics <- lapply(band$lyrics, function(x) unlist(str_split(x, ",")))
band$lyrics <- unlist(band$lyrics, use.names = FALSE)

lyrics_tab <- as.data.frame(table(factor(band$lyrics)))
names(lyrics_tab) <- c("lyric","count")
