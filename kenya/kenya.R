library(tidyverse)
library(extrafont)
library(cowplot)

# House generating function -------
make_house <- function(pct) {
  lower_bound <- 0.1
  upper_bound <- 0.9
  
  house_bg <- data.frame(
    x = c(lower_bound, lower_bound, 0, 0.5, 1, upper_bound, upper_bound, lower_bound),
    y = c(0, 1, 1, 1.5, 1, 1, 0, 0),
    g = 'bg'
  )
  
  if (pct < lower_bound & pct >= 0) {
    house_pct <- data.frame(
      x = c(0, pct, pct, 0),
      y = c(1, pct + 1, 1, 1),
      g = 'fg'
    )
  }
  
  else if (pct <= 0.5 & pct >= lower_bound) {
    house_pct <- data.frame(
      x = c(lower_bound, lower_bound, 0, pct, pct, lower_bound),
      y = c(0, 1, 1, pct + 1, 0, 0),
      g = 'fg'
    )
  }
  
  else if (pct > 0.5 & pct <= upper_bound) {
    house_pct <- data.frame(
      x = c(lower_bound, lower_bound, 0, 0.5, pct, pct, lower_bound),
      y = c(0, 1, 1, 1.5, 2 - pct, 0, 0),
      g = 'fg'
    )
  }
  
  else if (pct > upper_bound & pct <= 1.0) {
    house_pct <- data.frame(
      x = c(lower_bound, lower_bound, 0, 0.5, pct, pct, upper_bound, upper_bound),
      y = c(0, 1, 1, 1.5, 2 - pct, 1, 1, 0),
      g = 'fg'
    )
  }
  
  else {
    stop("Incompatible pct")
  }
  
  return(rbind(house_bg, house_pct))
}


# Get data -----
roof_material <- read_csv('v4212.csv')

# Clean up a little
roof_material <- roof_material %>% filter(AdminArea == "County") %>% 
  janitor::clean_names()


# Plot --------
roof_colors <- c("bg" = "#fafafaff", "title" = "black", "panel_text" = "black",
                 "house_pct" = "#f16b5cff", "house_bg" = "#586b4aff")

p1 <- roof_material %>% 
  group_by(county, ironsheets) %>% 
  summarize(
    x = make_house(ironsheets/100)$x,
    y = make_house(ironsheets/100)$y,
    g = make_house(ironsheets/100)$g
  ) %>% 
  ggplot(aes(x, y, group = interaction(county, g), fill = g)) + 
  geom_polygon() + 
  labs(
    title = "Ironsheet Roofs in Kenya",
    subtitle = "Each house represents the share of households in that\ncounty that have a steelsheet rooftop.",
    caption = "Source: {rKenyaCensus}    |    Visualization: @charliegallaghr"
  ) + 
  scale_fill_manual(name = "Legend:", values = c(roof_colors[["house_bg"]], roof_colors[["house_pct"]]),
                    labels = c("Other roof", "Ironsheet roof")) +
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(county)) + 
  theme_void(base_family = "Roboto Lt") + 
  theme(
    plot.margin = margin(25, 25, 10, 25),
    plot.background = element_rect(fill = roof_colors['bg'], color = NA),
    plot.title = element_text(color = roof_colors[["title"]],
                              margin = margin(10, 0, 0, 0),
                              size = 30),
    plot.subtitle = element_text(color = roof_colors[["title"]],
                                 margin = margin(10, 0, 40, 0),
                                 size = 16),
    plot.caption = element_text(color = roof_colors[["title"]],
                                margin = margin(10, 0, 5, 0),
                                size = 10),
    panel.spacing.y = unit(45, units = 'pt'),
    panel.spacing.x = unit(25, units = 'pt'),
    legend.box.margin = margin(0, 5, 0, 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 10),
  )

p2 <- ggdraw(p1) + 
  draw_image(image = "kenya_flag_icon.png", height = 0.07, x = 0.21, y = 0.05)

ggsave('test.png', p2, width = 10, height = 8, scale = 1.5)
