library(tidyverse)
library(cowplot)
library(extrafont)

beer <- readr::read_csv(
  paste0('https://raw.githubusercontent.com/',
         'rfordatascience/tidytuesday/master/data/2020/2020-10-20/',
         'beer_awards.csv')
  )


beer <- beer %>% 
  mutate(
    gold = medal == "Gold",
    silver = medal == "Silver",
    bronze = medal == "Bronze"
  ) %>%
  group_by(year, state) %>% 
  summarize(
    n_medals = sum(gold + silver + bronze)
  ) %>% 
  group_by(state) %>% 
  mutate(
    cum_medals = cumsum(n_medals),
    wis = state %in% c("WI", "CA", "CO")
  )

# Add a 2021 with the same value as 2020, for step-plot purposes
beer_2021 <- beer %>% 
  filter(year == 2020) %>% 
  mutate(
    year = 2021
  )


# A circle for labels
circle <- tibble(
  x = c(2000, 1996, 1990),
  y = c(750, 475, 300),
  size = c(70, 45, 30),
  lab_1 = c("CALIFORNIA", "COLORADO", "WISCONSIN"),
  lab_2 = c("California has won\nby far the greatest\nnumber of medals,\nwith 962.",
            "Colorado has won\n 659 medals.",
            "Until 2013, Wisconsin\n was in third place.\nThen their medal\nwinning began to\nslow down.")
)


p1 <- beer %>% 
  rbind(beer_2021) %>% 
  ggplot() + 
  geom_step(aes(x = year, y = cum_medals, group = state, color = wis)) +
  geom_point(data = circle, aes(x = x, y = y, size = size), color = "black") + 
  geom_text(data = circle, 
            aes(x = x, y = y + c(50, 40, 30), label = lab_1),
            size = c(6, 5, 4.5),
            family = 'Roboto Lt',
            color = 'white') + 
  geom_text(data = circle, 
            aes(x = x, y = y - c(15, 0, 21), label = lab_2),
            size = c(4, 4, 3),
            family = 'Roboto Lt',
            color = 'white',
            lineheight = 0.9) + 
  # California
  annotate(geom = "line", 
           x = c(2007, 2015), 
           y = c(750, 750),
           size = 0.6,
           color= grey(0.6)) +
  # Colorado
  annotate(geom = "line",
           x = c(2001.5, 2012.5),
           y = c(460, 460),
           size = 0.6, 
           color = grey(0.6)) +
  # Wisconsin
  annotate(geom = "line",
           x = c(1994.5,2011.5, 2011.5),
           y = c(300, 300, 200),
           size = 0.6, 
           color = grey(0.6)) +
  labs(
    title = "BEER MEDALS OVER TIME",
    subtitle = "Every year, the Great American Beer Festival awards medals to\nthe best American beers. This step plot illustrates the number\nof winning beers that came from each state over time.",
    caption = "Source: Great American Beer Festival  |  Visualization: @charliegallaghr"
  ) + 
  scale_color_manual(values = c(grey(0.65), 'black')) + 
  scale_radius(range = c(45,70)) + 
  scale_x_continuous(breaks = seq(1990, 2020, 10),
                     expand = c(0,1)) + 
  scale_y_continuous(name = "Medals Won", expand = c(0,0)) + 
  guides(color = FALSE, size = FALSE) + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    plot.title = element_text(family = "Oswald SemiBold",
                              color = 'black',
                              size = 28),
    plot.subtitle = element_text(family = 'Roboto Lt',
                                 color = 'black',
                                 size = 14),
    plot.caption = element_text(family = "Roboto Lt", 
                                size = 10,
                                color = grey(0.5)),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

svg(filename = "beer_fest.svg", width = 7, height = 10)
p1
dev.off()


# 
# 
# p2 <- beer %>% 
#   group_by(state) %>% 
#   summarize(
#     n_medals = sum(n_medals)
#   ) %>% 
#   ggplot() +
#   geom_col(aes(y = reorder(state, n_medals), x = n_medals))
# 
# 
# plot_grid(p1, p2)
