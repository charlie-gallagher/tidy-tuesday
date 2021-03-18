library(tidyverse)
library(cowplot)
library(extrafont)
library(lubridate)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


# Make basic data --------

# Unnecessary months conversion
months <- c("January" = 1, "February" = 2, "March" = 3,
            "April" = 4, "May" = 5, "June" = 6, "July" = 7, 
            "August" = 8, "September" = 9, "October" = 10, 
            "November" = 11, "December" = 12)

# Some of the biggest games
spec_games <- c(
  "PLAYERUNKNOWN'S BATTLEGROUNDS",
  "Dota 2",
  "Counter-Strike: Global Offensive"
)

# Add variables
games <- games %>% 
  mutate(
    month_num = months[month],
    date = ymd(paste(year, month, '01', sep = '-')),
    special_games = case_when(
      gamename %in% spec_games ~ gamename,
      TRUE ~ "Other"
    ),
    special_games = factor(special_games, levels = c(spec_games[1],
                                                     spec_games[2],
                                                     spec_games[3],
                                                     "Other"))
  )

# Colors
game_col <- c(
  "bg" = "#171a21ff",
  "fg" = "#15324bff",
  "title" = "#c5c3c0ff",
  "subtitle" = "#a4b4e3ff",
  "blurb" = "#a4b4e3ff",
  "text" = "#485e9eff",
  "cs" = "#f8766dff",
  "dota" = "#00bfc4ff",
  "pubg" = "#c77cffff",
  "other" = "#54527977"
)

# Subtitle text
game_blurb <- paste(
  "Steam is a PC gaming platform and community. On Steam, you",
  "can launch a game, download games, read and write reviews,",
  "and join a community of gamers all interested in giving and",
  "getting the best PC gaming experience.",
  sep = "\n"
)

# Blurb text
game_subtitle = paste(
  "This graphic explores the games with the greatest average",
  "number of players online since 2012, when Steam launched.",
  "Some games, driven by competitive gaming and frequent",
  "updates, have routinely hosted more than 250,000 gamers at",
  "once, on average, for over a decade.",
  sep = "\n"
)

# Plot labels
game_labels <- tibble(
  text = c("Counter-Strike: Global Offensive", "Dota 2", 
           "PLAYERUNKNOWN'S\nBATTLEGROUNDS"),
  x = c(16900, 16900, 17900),
  y = c(450000, 800000, 1000000),
  color = game_col[c("pubg", "dota", "cs")]
)




# Plot ----------

# Data plot
p1 <- games %>% 
  ggplot() + 
  geom_step(aes(x = date, y = avg, group = gamename, color = special_games),
            size = 1) + 
  geom_text(data = game_labels, aes(x = x, y = y, label = text),
            color = game_labels$color, size = c(4.25, 4.25, 3.85),
            family = "VCR OSD Mono") + 
  scale_color_manual(values = unname(game_col[c("cs", "dota", "pubg", "other")])) + # Must unname or else
  scale_y_continuous(expand = c(0,0), label = scales::comma_format(accuracy = 1)) + 
  scale_x_continuous(expand = c(0,0)) +
  guides(color = FALSE) +
  theme_void(base_family = "VCR OSD Mono") +
  theme(
    text = element_text(color = game_col[["text"]]),
    plot.margin = margin(80, 70, 50, 40),
    plot.background = element_rect(color = NA, fill = game_col[["bg"]]),
    axis.text.y = element_text(color = game_col[["text"]])
  )



# Complete image
fp <- ggdraw(p1) + 
  draw_line(x = c(0, 1), y = 0.01, size = 20, color = game_col[["fg"]]) +
  draw_line(x = c(0.175, 0.5), y = 0.89, color = game_col[["fg"]], size = 80) +
  draw_text("The Rise and Rise\nof Counter-Strike", x = 0.2, y = 0.8, size = 30, 
            color = game_col[["title"]], family = "VCR OSD Mono", 
            lineheight = 0.9, hjust = 0) + 
  draw_text(game_subtitle, x = 0.2, y = 0.69, size = 10, color = game_col[["subtitle"]],
            family = "Roboto Lt", lineheight = 0.9, hjust = 0) + 
  draw_text("Source: Steam and SteamCharts   |   Visualization: @charliegallaghr",
            family = "Roboto Lt", size = 10, x = 0.98, y = 0.025, color = game_col[["blurb"]],
            hjust = 1)

ggsave("games.png", plot = fp, width = 11, height = 6, scale = 1.35)

