# Cleaning file from TidyTuesday -------------------
library(tidyverse)
library(rvest)

url_wsj <- "https://raw.githubusercontent.com/WSJ/measles-data/master/all-measles-rates.csv"

wsj <- read_csv(url_wsj)

list_of_urls <- "https://github.com/WSJ/measles-data/tree/master/individual-states"

raw_states <- list_of_urls %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(Name) %>% 
  mutate(Name = str_remove(Name, "\\.csv")) %>% 
  filter(str_length(Name) > 3, str_length(Name) < 20) %>% 
  pull(Name)
# This section gets the file names for each of the 50 csv files at list_of_urls. 
# 

all_states <- glue::glue("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/{raw_states}.csv") %>% 
  map(read_csv)

clean_states <- all_states %>% 
  map(~select(., state, name, lat, lng)) %>% 
  map(~mutate_at(., vars(lat, lng), as.numeric)) %>% 
  bind_rows() %>% 
  filter(!is.na(lat))

wsj %>% 
  left_join(clean_states, by = c("name", "state")) %>% 
  write_csv(here::here("2020","2020-02-25","measles.csv"))



# My own takeaways --------------
ariz <- read_csv("https://github.com/WSJ/measles-data/blob/master/individual-states/arizona.csv")
# ooooh so you can read csv files from the internet really easily. 

# Glue
glue::glue("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/{raw_states}.csv")
# Equivalent, more or less. Technically, glue::glue produces an object of class 'glue','character'
paste0("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/", raw_states, ".csv")
