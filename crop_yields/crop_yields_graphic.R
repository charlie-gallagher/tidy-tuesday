library(tidyverse)
library(ggflags)
library(extrafont)
library(lubridate)

# Fonts ----------
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.52/bin/gswin64c.exe")
loadfonts()


# Data -----------
# Crop yields
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
key_crop_yields <- janitor::clean_names(key_crop_yields) %>% 
  mutate(date = ymd(paste0(year, '-01-01')))

# Extra country information
country_codes <- readr::read_csv('https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv') %>% 
  janitor::clean_names()
country_codes <- country_codes %>% 
  select(alpha3 = iso3166_1_alpha_3, alpha2 = iso3166_1_alpha_2, 
         region = sub_region_name, sub_region_code, developed = developed_developing_countries)



crop_diff <- key_crop_yields %>% 
  left_join(country_codes, by = c('code' = 'alpha3')) %>% 
  filter(
    date %in% c(ymd('1961-01-01'), ymd('2018-01-01')), 
    !is.na(alpha2),
    !is.na(sub_region_code)
  ) %>% 
  select(entity, code, alpha2, date, wheat_tonnes_per_hectare,
         sub_region_code, developed) %>% 
  filter(!is.na(wheat_tonnes_per_hectare)) %>% 
  pivot_wider(id_cols = c(entity, code, alpha2, sub_region_code, developed), 
              values_from = wheat_tonnes_per_hectare,
              names_from = date
  ) %>% 
  rename(wheat_1961 = `1961-01-01`,
         wheat_2018 = `2018-01-01`) %>% 
  filter(!is.na(wheat_1961),
         !is.na(wheat_2018))


mean_wheat <- crop_diff %>% 
  pivot_longer(cols = c(wheat_1961, wheat_2018), names_to = "year", values_to = "wheat") %>% 
  group_by(entity) %>% 
  mutate(mean_wheat = mean(wheat, na.rm = TRUE),
         mean_wheat = case_when(developed == "Developed" ~ mean_wheat * 10,
                                developed == "Developing" ~ mean_wheat)) %>% 
  pull(mean_wheat)
mean_wheat <- mean_wheat[seq(1, 2 * nrow(crop_diff), 2)]


cairo_pdf("crop.pdf", width = 7, height = 14)
crop_diff %>% 
  mutate(diff_ind = wheat_2018 - wheat_1961 > 0) %>% 
  ggplot() + 
  geom_segment(aes(x = wheat_1961, xend = wheat_2018, 
                   y = reorder(entity, -mean_wheat), yend = entity,
                   color = diff_ind),
               size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("red","#dbc1ac")) + 
  geom_flag(mapping = aes(x = wheat_1961, y = entity, country = tolower(alpha2))) + 
  geom_flag(mapping = aes(x = wheat_2018, y = entity, country = tolower(alpha2))) + 
  scale_x_continuous(name = "Wheat Production (Tonnes per Hectare)") +
  theme_minimal() + 
  theme(
    text = element_text(family = 'Arapey', color = "#dbcbbd"),
    plot.background = element_rect(fill = "#3E3232", color = NA),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 10),
    panel.background = element_rect(fill = '#3E3232', color = NA),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dotted', color = "#5e534f",
                                      size = 0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, color = "#dbcbbd"),
    axis.text.x = element_text(size = 10, color = "#dbcbbd"),
    axis.title.x = element_text(size = 15)
  )
dev.off() 

