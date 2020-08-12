# Get coffee
coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
save(coffee, file = 'data/coffee.RData')


library(tidyverse)
library(skimr)
library(janitor)

skimr::skim(coffee)
tabyl(coffee, species)
