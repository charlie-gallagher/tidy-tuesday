library(tidyverse)
library(friends)
library(skimr)
library(extrafont)
library(cowplot)

# Fonts
loadfonts(device = 'win')
loadfonts(device = 'postscript')

# Import data
f_emotions <- friends::friends_emotions
f_entities <- friends::friends_entities
f_info <- friends::friends_info
f <- friends::friends

friends <- c('Chandler Bing','Ross Geller','Monica Geller','Rachel Green',
             'Joey Tribbiani','Phoebe Buffay')


# Theme
friends_theme <- theme(
  text = element_text(family = 'Roboto Lt', face = 'plain', color = 'white',
                      size = 40),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = '#182326', color = NA),
  plot.margin = margin(0, 200, 100, 200, unit = 'pt')
)



# Graphic one ----
g1_colors <- c('#305e6a','#00b2dd','#f51520','#ee8d7a','#ffd600','#fff7a2')
g1 <- f %>% 
  filter(speaker %in% friends) %>% 
  group_by(season, episode, speaker) %>% 
  summarize(n_lines = n()) %>% 
  left_join(f_info, by = c('episode','season')) %>% 
  group_by(season, episode) %>% 
  mutate(
    total_lines = sum(n_lines),
    share_lines = n_lines / total_lines,
    unity = sum(share_lines)
  ) %>% 
  ungroup() %>% 
  mutate(
    seas_epi = factor(paste(season, episode, sep = '-'), ordered = TRUE)
  ) %>% 
  ggplot() + 
  geom_area(aes(x = seas_epi, y = share_lines, group = speaker, fill = speaker)) +
  scale_fill_manual(name = "Character", values = g1_colors) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(name = "Episode") + 
  theme_void() + 
  friends_theme + 
  theme(
    plot.margin = margin(0, 0, 50, 350),
    axis.title.x = element_text(margin = margin(10, 2, 2, 2))
  )


# Graphic two ----

writers_all <- f_info %>% 
  pull(written_by) %>% 
  str_replace(" ", "") %>% # unusual symbol after "Story by"
  str_replace(" ", "") # unusual symbol after "Teleplay by"
# I don't know what this symbol is, but it's always there

# Replace 
writers <- str_replace_all(writers_all, "Story by: ", "")
writers <- str_replace_all(writers, "Teleplay by:", " & ")

# Sometimes, names are written "First LastFirst Last". This expression 
# gets rid of those. A simpler expression could split McCarthy by mistake
writers <- str_replace_all(writers, "([a-z]{2})([A-Z][a-z])",
                           "\\1 & \\2") 
writers <- str_replace_all(writers, "Jill Condon\n& Amy Toomin",
                           "Jill Condon & Amy Toomin")

# Convert to df and add to a new dataset
writers_df <- str_split(writers, "\\s*&\\s*", simplify = TRUE) %>% as.data.frame()
names(writers_df) <- c('first_writer','second_writer','third_writer',
                       'fourth_writer','fifth_writer')

f_info_writers_wide <- cbind(f_info, writers_df) %>% as_tibble()
f_info_writers <- cbind(f_info, writers_df) %>% as_tibble() %>% 
  pivot_longer(cols = c('first_writer','second_writer',
                        'third_writer','fourth_writer',
                        'fifth_writer'),
               names_to = c('writer_number'),
               values_to = c('writer_name'))

# Summarize
top_20_writers <- f_info_writers %>% 
  group_by(writer_name) %>% 
  summarize(n_episode = n()) %>% 
  arrange(desc(n_episode)) %>% 
  filter(writer_name != "") %>% 
  slice_head(n = 20) %>% 
  pull(writer_name)


# graphic
g2 <- f_info_writers %>% 
  mutate(is_new_writer = !duplicated(writer_name)) %>% 
  group_by(air_date) %>% 
  summarize(is_new_writer = sum(is_new_writer)) %>% 
  mutate(cum_is_new_writer = cumsum(is_new_writer)) %>% 
  ggplot() + 
  geom_step(aes(x = air_date, y = cum_is_new_writer),
            color = "#ffd600", size = 1) + 
  scale_x_continuous(name = "Episode", expand = c(0,0)) + 
  scale_y_continuous(name = "Cum. Number of Writers", expand = c(0,0)) + 
  theme_void() + 
  friends_theme + 
  theme(
    axis.line.x = element_line(color = 'white', size = 0.75),
    axis.line.y = element_line(color = 'white', size = 0.75),
    axis.text.y = element_text(margin = margin(5, 5, 5, 5)),
    panel.grid.major.y = element_line(color = grey(0.5), linetype = '18',
                                      size = 1),
    plot.margin = margin(100, 200, 100, 380),
    axis.title.x = element_text(margin = margin(10, 2, 2, 2)),
    axis.title.y = element_text(margin = margin(2, 2, 2, 2), angle = 90)
  )


# Graphic three -------
f_writers <- f %>% 
  left_join(f_info_writers_wide, by = c('season','episode')) %>% 
  pivot_longer(cols = c('first_writer','second_writer',
                        'third_writer','fourth_writer',
                        'fifth_writer'),
               names_to = c('writer_number'),
               values_to = c('writer_name')) %>% 
  filter(writer_name != "", speaker %in% friends) %>% 
  group_by(writer_name, speaker) %>% 
  summarize(n_lines = n()) %>% 
  arrange(writer_name, desc(n_lines))

# Generating variables based on line number
rank_values <- c('#f50d1a', rep('#464d4f', 4), '#00b2dd')

g3 <- f_writers %>% 
  group_by(writer_name) %>% 
  mutate(sum_lines = sum(n_lines, na.rm = TRUE),
         pct_lines = n_lines / sum_lines,
         rank = rank(n_lines, ties.method = 'first')) %>% 
  filter(writer_name %in% top_20_writers) %>% 
  ggplot() + 
  geom_col(aes(x = speaker, y = 1, 
               fill = factor(rank, labels = c('Fewest lines',' ','  ','   ','     ','Most lines')))) + 
  facet_grid(rows = vars(writer_name)) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(position = 'top') + 
  scale_fill_manual(values = rank_values, name = "Rank") + 
  theme_void() + 
  friends_theme + 
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, face = 'bold', hjust = 0),
    plot.margin = margin(100, 100, 100, 100, unit = 'pt')
  )



# Title plot ---------
title_df <- tibble(
  x = c(1, 18, 1),
  y = c(8, 8, 7),
  label = c("FRIENDS", "An Analysis of the Writer's Room", 
            "Fifty-one writers contributed to \"Friends\" during its 236-episode run. This info-graphic looks\nat the writer's room over its history and examines some of the trends in writing habits of the\nwriting staff and individual writers.")
)

title_line <- tibble(
  x = c(1.1, 37),
  y = c(7.5, 7.5)
)

title_plot <- title_df %>% 
  ggplot() + 
  geom_text(aes(x = x, y = y, label = label),
            size = c(80, 20, 15), hjust = 0, vjust = c(0, 0, 1),
            family = c('Roboto Cn', 'Palatino Linotype', 'Roboto Lt'),
            color = 'white', fontface = c('bold', 'plain','plain')) + 
  geom_line(data = title_line, aes(x = x, y = y), size = 1.5, color = 'white') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 15)) + 
  theme_void() + 
  friends_theme + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(100, 0, 100, 200, unit = 'pt')
  )


# Combined graphic
png(filename = 'friends_plots.png', width = 3500, height = 2400, units = 'px')
left_grid <- plot_grid(title_plot, g1, g2, nrow = 3, rel_heights = c(1, 1, 1))
full_grid <- plot_grid(left_grid, g3, ncol = 2, rel_widths = c(1.5, 1))

ggdraw(full_grid) + 
  draw_label("This panel explores the\nshare of each episode's lines\nheld by each character.There\nis a surprising evenness\nacross the whole show.",
             color = 'white', x = 0.1, y = 0.63, hjust = 1, size = 25,
             fontfamily = 'Roboto') + 
  draw_label("The show-runners were\nconstantly adding writers. This\ngraphic shows the cumulative\nnumber of writers throughout\nthe show's 236 episodes.",
             color = 'white', x = 0.105, y = 0.25, hjust = 1, size = 25,
             fontfamily = 'Roboto') + 
  draw_label("This panel investigates\ncharacter favoritism among\nthe most frequent contributors.\nEach writer has a character\nprofile which shows which\ncharacter they wrote the most\nfor, and which they wrote\nthey wrote the least for. Rachel\nand Ross were frequent\nfavorites, while Phoebe was\noften given the fewest lines.",
             color = 'white', x = 0.88, y = 0.805, hjust = 0, vjust = 1, size = 28,
             fontfamily = 'Roboto') + 
  draw_label("Source: {friends} package  |  Visualization: @charliegallaghr",
             color = grey(0.5), x = 0.7, y = 0.015, size = 25, fontfamily = 'Roboto')
dev.off()
