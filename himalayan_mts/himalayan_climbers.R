library(tidyverse)
library(extrafont)


expeditions <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')


# Arranging values in a simplified (and aesthetically necessary) order
termination_reason_values <- expeditions %>% pull(termination_reason) %>% unique()
termination_reason_values <- 
  termination_reason_values[c(3,2,4,5,8,9,11,13,1,7,10,6,14,12,15)]

termination_reason_labels <- termination_reason_values %>% 
  replace(c(7,8,9), "Success") %>% 
  replace(c(2,3), "Bad weather or conditions") %>% 
  replace(c(11,12,13,14,15), "Other")


# Bar colors
mtn_colors <- c('#ffffffff','#aec0ceff','#62728cff','#7acbefff',
                '#225997ff','#003062ff','#6a8550ff','#626262ff')


# Data work
exp <- expeditions %>% 
  filter(peak_id == "EVER") %>%  
  mutate(
    reason = termination_reason,
    reason = factor(termination_reason, 
                    levels = termination_reason_values,
                    labels = termination_reason_labels)
  ) %>% 
  arrange(year, reason) %>%
  select(expedition_id, peak_id, peak_name, year, termination_reason, reason) %>% 
  mutate(id = row_number())



# Plot
p <- exp %>% 
  ggplot() + 
  geom_bar(aes(x = year, group = id, fill = reason), color = "#101924ff") + 
  annotate(geom = "text", x = 1990, y = 45, 
           label = "In 2014, an avalanche in the notorious Khumbu Icefall killed\nsixteen Sherpas and injured nine. The other Sherpas, protesting\ntheir government's lack of support, announced they would not\nclimb for the rest of the season.",
           hjust = 1, color = grey(0.9), family = "Roboto Lt",
           size = 3, lineheight = 0.9) +
  annotate(geom = "curve", 
           x = 1990.5, xend = 2014, 
           y = 45, yend = 25,
           size = 0.25,
           curvature = -0.2, 
           color= grey(0.9)) + 
  annotate(geom = "text", x = 1993, y = 65,
           label = "In 2015, an earthquake triggered severe avalanches that killed\nat least twenty-two people at the Everest Base Camp. At\nleast 61 people were injured.",
           hjust = 1, color = grey(0.9), family = "Roboto Lt",
           size = 3, lineheight = 0.9) + 
  annotate(geom = "curve", 
           x = 1993.5, xend = 2015, 
           y = 65, yend = 45,
           size = 0.25,
           curvature = -0.2, 
           color= grey(0.9)) + 
  geom_point(data = data.frame(x = c(2014,2015), y = c(25, 45)),
                               aes(x = x, y = y),
             color = grey(0.9), size = 1.5) + 
  labs(
    title = "EVEREST EXPEDITIONS",
    subtitle = "Journeys to the top of the world end for many reasons. This graphic\nlooks at the results of all Everest expeditions since 1921.\nEverest has had a colorful history, but even\nstill most expeditions reach the peak.",
    caption = "Source: The Himalayan Database  |  Visualization: Charlie Gallagher"
  ) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(name = "Number of Expeditions", expand = c(0,0)) + 
  scale_fill_manual(values = mtn_colors) +
  theme_bw() + 
  theme(
    text = element_text(family = "Roboto Lt", 
                        color = grey(0.9)),
    title = element_text(family = "Cambo", size = 30),
    rect = element_rect(fill = "#101924ff", color = NA),
    panel.background = element_rect(fill = "#101924ff", color = NA),
    panel.border = element_blank(),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(10, "pt"),
    legend.key.width = unit(40, "pt"),
    legend.position = c(0.24, 0.92),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#252f39ff"),
    plot.margin = margin(20, 40, 5, 40),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0,0,0,0)),
    plot.subtitle = element_text(family = "Roboto Lt", 
                                 size = 12, margin = margin(0,0,20,0)),
    plot.caption = element_text(family = "Roboto", size = 8, color = grey(0.4)),
    axis.title.y = element_text(family = "Roboto Lt", size = 10,
                                color = grey(0.9)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = grey(0.4)),
    axis.text.y = element_text(color = grey(0.4)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

svg(filename = "everest.svg", width = 11, height = 7)
p
dev.off()
  