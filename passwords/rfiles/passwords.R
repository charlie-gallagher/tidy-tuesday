library(tidyverse)
library(ggrepel)
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
save(passwords, file = "data/passwords.RData")


#---------------------------------------------------------------------------
load("data/passwords.RData")
head(passwords)

ggplot(passwords) + 
  geom_text(aes(x = offline_crack_sec, y = rank, label = password)) + 
  scale_x_log10()
# This isn't very useful yet. I can recreate the online graph by ranking the 
# alphanumeric names from a-z, 0-9.  

passwords %>%
  arrange(password) %>%
  mutate(
    alphanum = seq_along(password)
  ) %>%
  ggplot() + 
  geom_text(aes(x = alphanum, y = rank, label = password,
                size = strength)) + 
  scale_size(range = c(2,10))


passwords %>%
  arrange(password) %>%
  mutate(
    alphanum = seq_along(password)
  ) %>%
  ggplot() + 
  geom_text_repel(aes(x = alphanum, y = rank, label = password, size = font_size),
                  segment.color = NA)


#-----------------------------------
# Ideas? I've got popularity, the password itself, its category, time to crack
# both online and offline (value and offline_crack_sec, with time unit), 
# another rank that I don't know yet, strength, and font size (oo, nice). 
# 
# 
# 
# 
# 

# What's better, ratio of letters and numbers or length? 
# To make the ratio, I can count the number of numbers (0-9) and subtract
# from the total length to get the string. There are no spaces, so piece
# of cake. What about string length? 
passwords$length <- str_length(passwords$password)
plot(passwords$length, passwords$strength)

library(stringr)
passwords$numchar <- str_count(passwords$password, pattern = "[a-z]|[A-Z]")
passwords$numnum <- str_count(passwords$password, pattern = "[0-9]")
passwords <- passwords %>%
  mutate(
    numchar = str_count(passwords$password, pattern = "[a-z]|[A-Z]"),
    numnum = str_count(passwords$password, pattern = "[0-9]"),
    composite = numchar * numnum
  )


nogrid <- theme(
  panel.grid = element_blank(),
  panel.background = element_rect(color = NA),
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(color = "black")
)

pw_plot <- passwords %>% 
  ggplot() + 
  geom_jitter(aes(x = composite, y = strength, color = category)) + 
  theme_bw()


pw_plot + 
  nogrid + 
  labs(title = "Composite score for alphanumerosity versus strength")
