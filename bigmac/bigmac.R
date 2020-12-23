library(tidyverse)
library(lubridate)   # as_datetime
library(extrafont)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

# Generate in-betweens ------
big_mac_inbetween <- big_mac %>% 
  group_by(iso_a3) %>% 
  mutate(
    date = as.numeric(as_datetime(date)),  # easier to interpolate a numeric var
    usd_lead = lead(usd_adjusted),
    date_lead = lead(date),
    # Solving for x given two points and y = 0
    xzero = -((usd_adjusted * (date_lead - date)) / (usd_lead - usd_adjusted)) + date, 
    xzero_valid = xzero > date & xzero < date_lead,
    yzero = 0,
    xzero = replace(xzero, !xzero_valid, NA),
    yzero = replace(yzero, !xzero_valid, NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(xzero)) %>% 
  select(date = xzero, name, usd_adjusted = yzero)

# Append to existing data -----
big_mac_area <- big_mac %>% 
  mutate(date = as.numeric(as_datetime(date))) %>% 
  select(date, name, usd_adjusted) %>% 
  rbind(big_mac_inbetween) %>% 
  mutate(date = as_datetime(date)) %>% 
  filter(!is.na(usd_adjusted) & name != "United States")

# Make facet panel titles dataset --------
big_mac_area_titles <- big_mac_area %>% 
  select(name) %>% unique() %>% 
  mutate(x = as_datetime(ymd('2016-01-01')), y = 1.18)


# Plotting ---------
bigmac_subtitle_text <- paste(
  "The Economist's Big Mac Index is based on the idea of Purchasing Power Parity (PPP), that over time a country's",
  "currency should tend towards having the same buying power as another's, given an identical basket of goods. Big",
  "Macs are highly standardized, which makes them a surprisingly good indicator for purchasing power. Using the cost",
  "of a local Big Mac relative to how much it \"should\" cost given the USD exchange rate and the cost of a Big Mac in the",
  "US, Economists have a fun indicator for whether a currency is over- or under-valued. This Graphic shows the (GDP-",
  "adjusted) Big Mac indices to illustrate how over- or under-valued a currency appears to be.",
  sep = '\n'
)

big_mac_area %>% 
  ggplot(aes(x = date, y = usd_adjusted)) + 
  geom_area(data = filter(big_mac_area, usd_adjusted >= 0), fill = '#393e46') + 
  geom_area(data = filter(big_mac_area, usd_adjusted <= 0), fill = '#ffbe20ff') + 
  geom_text(data = big_mac_area_titles,
            aes(x = x, y = y, label = name),
            family = 'Roboto Cn', fontface = 'bold', size = 4.5) + 
  labs(
    title = "Big Mac Index",
    subtitle = bigmac_subtitle_text,
    caption = "Source: The Economist    |    Visualization: @charliegallaghr"
  ) + 
  facet_wrap(vars(name)) + 
  theme_void() + 
  theme(
    plot.margin = margin(3, 2.5, 0.5, 2.5, unit = 'lines'),
    plot.background = element_rect(fill = "#efefef", color = NA),
    plot.title = element_text(family = 'Roboto Lt', size = 50, margin = margin(0, 0, 7.5, 0)),
    plot.subtitle = element_text(family = "Roboto Cn", size = 12, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(family = "Roboto", size = 12, color = '#898989'),
    panel.spacing.x = unit(2.7, 'lines'),
    panel.spacing.y = unit(2, 'lines'),
    strip.text = element_blank(),
  ) + ggsave('bigmac.png', width = 10, height = 12, dpi = 'retina')
