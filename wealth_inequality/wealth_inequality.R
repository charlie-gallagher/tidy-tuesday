library(tidyverse)
library(extrafont)
library(cowplot)
library(ggtext)

race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

# TODO: Reduce the code to what is actually used

# Make data ----
id <- income_distribution %>% 
  filter(year == 2016, 
         !(race %in% c('Asian Alone', 'Black Alone', 'White Alone')),
         race != "All Races"
  ) %>% 
  group_by(race, number, income_mean) %>% 
  summarize() %>% 
  ungroup() %>% 
  mutate(
    total_income = number * income_mean,
    share_pop = number / sum(number),
    share_inc = total_income / sum(total_income)
  )

wealth <- id %>% 
  mutate(
    race = case_when(
      grepl("White", race) ~ "White",
      grepl("Black", race) ~ "Black",
      grepl("Hispanic", race) ~ "Hispanic"
    )
  ) %>% 
  select(race, number, total_income)

w <- race_wealth %>% 
  filter(type == "Average" & year == 2016 & race != "Non-White") %>% 
  select(-type) %>% 
  left_join(wealth, by = 'race') %>% 
  mutate(
    share_pop = number / sum(number),
    share_inc = total_income / sum(total_income),
    total_wealth = wealth_family * number,
    share_wealth = total_wealth / sum(total_wealth),
    race = factor(race, levels = c("Hispanic", "Black", "White"))
  ) %>% 
  select(year, race, share_pop, share_inc, share_wealth) %>% 
  pivot_longer(cols = share_pop:share_wealth)


# Plot -----

## Color palette ----
w_col <- c(
  "bg" = "#edf2f5",
  "main_text" = "#00243f",
  "dim_text" = "#3c5b74",
  "plot_text" = "#ffffff",
  "white" = "#da9245ff",
  "black" = "#3c5b74",
  "hispanic" = "#93a9ba"
)

## Data ----
w1 <- w %>% 
  filter(name %in% c('share_pop', 'share_wealth')) %>% 
  group_by_all() %>% 
  summarize(
    x = case_when(
      name == "share_pop" ~ c(0,1),
      name == "share_wealth" ~ c(5, 6)
    )
  ) %>% 
  ungroup()

w_text <- w %>% 
  filter(name != "share_inc") %>% 
  group_by(name) %>% 
  mutate(y = cumsum(value) - (value / 2)) %>% 
  ungroup() %>% 
  mutate(
    x = case_when(
      name == "share_pop" ~ 0.5,
      name == "share_wealth" ~ 5.5
    ),
    x = ifelse(value < 0.1, 6.07, x),
    text = paste0(round(value * 100, digits = 0), "%"),
    family = c("Montserrat Light"),
    size = c(4),
    hjust = c(0.5, 0.5, 0.5, 0, 0.5, 0),
    vjust = c(0.5),
    color = c(rep(w_col[["plot_text"]], 3), w_col[["main_text"]], 
              w_col[["plot_text"]], w_col[["main_text"]])
  )


## plot -----
p1 <- w1 %>% 
  ggplot() + 
  geom_area(aes(x = x, y = value, group = race, fill = race),
            color = w_col[["bg"]], size = 1.3) + 
  geom_text(data = NULL, aes(x = -0.05, y = 0.5, label = 'Share of Households'),
            angle = 90, vjust = 0, family = "Montserrat Light",
            size = 4, color = w_col[["main_text"]]) +
  geom_text(data = NULL, aes(x = 6.05, y = 0.5, label = 'Share of Wealth'),
            angle = -90, vjust = 0, family = "Montserrat Light",
            size = 4, color = w_col[["main_text"]]) +
  geom_text(data = data.frame(), aes(x = -0.05, y = c(0.2, 0.784, 0.929),
                             label = c("White", "Black", "Hispanic")),
            family = "Montserrat Light", hjust = 1, color = w_col[["dim_text"]],
            size = 3) +
  geom_text(data = w_text, aes(x = x, y = y, label = text),
            family = w_text$family, size = w_text$size, 
            hjust = w_text$hjust, vjust = w_text$vjust,
            color = w_text$color) + 
  scale_fill_manual(values = c(w_col[["hispanic"]], w_col[["black"]], w_col[["white"]])) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0, 1, 0, 0.5)) +
  guides(fill = FALSE) +
  labs(caption = "Source: Urban Institute and the Census Bureau  |  Visualization: @charliegallaghr") + 
  theme_void(base_family = "Montserrat Light") + 
  theme(
    plot.margin = margin(20, 0, 5, 0),
    plot.caption = element_text(size = 7, hjust = 0.7,
                                color = w_col[["main_text"]],
                                margin = margin(10, 0, 0, 0)),
    plot.background = element_rect(color = NA, fill = w_col[["bg"]])
  )

# Text -------
## Data ----
subtitle_text <- paste(
  "White households in the United States",
  "compose 71% of white, Hispanic, and black",
  "households, but they hold 93% of the wealth.",
  "Wealth inequality in the United States is",
  "entrenched in a history of racial",
  "discrimination.",
  sep = "\n"
)

w_text_1 <- tibble(
  text = c("WEALTH\nINEQUALITY", subtitle_text),
  x = c(5, 5),
  y = c(9.5, 6.5),
  family = c("Abril Fatface", "Montserrat Light"),
  size = c(14, 4),
  hjust = c(0.5),
  vjust = c(1)
)

## Plot ------
p2 <- ggplot(w_text_1, aes(x = x, y = y, label = text)) + 
  geom_text(family = w_text_1$family, 
            size = w_text_1$size, 
            hjust = 0.5, vjust = 1,
            lineheight = 0.9,
            color = w_col[["main_text"]]) +
  xlim(c(0.5, 9.5)) + ylim(c(0, 10)) + 
  theme_void() + 
  theme(plot.background = element_rect(color = NA, fill = w_col[["bg"]]))

p_all <- plot_grid(p2, p1, nrow = 1, rel_widths = c(1, 1.3))

ggsave("house.png", p_all, width = 10, height = 5, type = 'cairo')

svg('house.svg', height = 5, width = 10)
p_all
dev.off()
