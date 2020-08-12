library(tidyverse)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

save(hotels, file = 'data/hotels.RData')

# here:: package --------------------------------
here::here()
  # Returns root file path, in this case the project location
here::here('data','rfiles')
  # Returns the root file path with two extensions


# Basic facts -----------------------------------
load(here::here('data','hotels.RData'))

skimr::skim(hotels)

# Two hotels: city and resort
dplyr::count(hotels, hotel)

# Potential good ideas: Compare basic facts about city and resort hotels; predict
# cancellations; use AGENT variable to compare different booking agencies (e.g.
# who gets their customers the best price?). This would be quite complicated,
# since some agencies might book more high-end rooms than others, leading
# to artificial differences. Typical econometric caveats apply. 

count(hotels, agent) # 334 different agencies, numeric ID. Cf. company.
count(hotels, assigned_room_type) # 12 types, alphabetic ID
count(hotels, is_canceled) # About 37% are cancellations
count(hotels, is_repeated_guest) # Around 3% are repeated guests
count(hotels, customer_type) # Most transient and transient-party; least group


summary(hotels$adr)
hotels %>%
  filter(adr < 1000) %>%
  ggplot() + 
  geom_histogram(aes(x = adr, y = stat(density), fill = factor(is_canceled)))




# Text manipulation challenge: convert camelCase to tidy_case (every capital 
# is a new word). One idea is a function that converts each letter in the 
# word to an 'f' or a 't' depending on whether it is a capital, then 
# replacing all 't's with dashes. That's probably not possible without parsing
# each letter or something, though... Hm. 
  # By the way, this is what janitor::clean_names() does, I believe. 

# Temporary camelCase to tidy_case challenge ------------
library(tidytext)
library(stringr)
library(stringi)

# Regular expressions can recognize [A-Z] which notices only capitals. Maybe
# this is the key? 

# A similar problem might be to double every capital, for example. 

# Stringi is a complex package, and I want to learn it. 

# The key to this is a backreference. Replace every capital with an underscore 
# and the same capital. 

camelCase <- c("ThisIsSomeCamelCase")
stri_replace_all_regex(camelCase,
                       "(?<=\\w)([A-Z])", 
                       "_$1") %>%
  tolower()
