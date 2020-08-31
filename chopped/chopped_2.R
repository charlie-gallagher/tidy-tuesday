# This inspired by https://twitter.com/Alocord/status/1298113816832679941. 
# Can I make the same plot but without using facets? To be more specific, I 
# want to use only four facets: season 1-10, 11-20, 21-30, 31-end

library(tidyverse)
library(janitor)

chop <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


# Season group variable for faceting

# How to make the x-axis data: 
# - every season group starts from 1
# - every season is advanced by a certain stable amount


mean_chop <- chop %>% 
  mutate(mean_chop = mean(episode_rating, na.rm = TRUE)) %>% 
  pull(mean_chop)
mean_chop <- unique(mean_chop)

chop2 <- chop %>% 
  filter(season <= 40) %>% 
  mutate(
    season_gp = case_when(
      season <= 15 ~ 1,
      season > 15 & season <= 30 ~ 2,
      season > 30 & season <= 45 ~ 3
    ),
    season_gp = factor(season_gp, labels = c("Seasons 1-15","Seasons 16-30",
                                             "Seasons 30-40"))
  ) %>% 
  group_by(season_gp) %>% 
  mutate(
    season_by_gp = season - (15 * as.numeric(season_gp)) + 15,
    xaxis = row_number() + (season_by_gp * 4) - 4
  ) %>% 
  select(season_episode, season, season_by_gp, season_gp, xaxis, episode_rating)

chop2 %>% 
  ggplot() + 
  geom_line(aes(x = xaxis, y = episode_rating, group = season), 
            color = 'white', alpha = 0.5, size = 1) + 
  geom_smooth(aes(x = xaxis, y = episode_rating), se = FALSE, 
              color = 'black', linetype = 'dashed', size = 1) + 
  geom_hline(yintercept = mean_chop, size = 0.5) + 
  scale_y_continuous(expand = c(0,0), limits = c(5, 10)) + 
  guides(color = FALSE) + 
  facet_grid(rows = vars(season_gp)) + 
  ggthemes::theme_hc() + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#e0861f"),
    plot.background = element_rect(fill = "#c26713"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y.left = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0, family = 'serif')
  )





# In this second version, no faceting, just a very wide graph
chop3 <- chop %>% 
  filter(season <= 40) %>% 
  mutate(
    xaxis = row_number() + (season * 4) - 4
  ) %>% 
  select(season_episode, season, xaxis, episode_rating)

chop3 %>% 
  ggplot() + 
  geom_line(aes(x = xaxis, y = episode_rating, group = season), 
            color = 'white', alpha = 0.5, size = 1) + 
  geom_smooth(aes(x = xaxis, y = episode_rating), se = FALSE, 
              color = 'black', linetype = 'dashed', size = 1) + 
  geom_hline(yintercept = mean_chop, size = 0.5) + 
  scale_y_continuous(expand = c(0,0), limits = c(5, 10)) + 
  scale_x_continuous(expand = c(0,0)) + 
  guides(color = FALSE) + 
  ggthemes::theme_hc() + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#c26713"),
    plot.background = element_rect(fill = "#c26713"),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color = "white", size = 15),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y.left = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0, family = 'serif')
  )

