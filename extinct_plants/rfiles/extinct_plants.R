library(tidyverse)
library(janitor)


load('data/extinct_plants.RData')

group_threats <- plants %>% 
  group_by(group) %>% 
  summarize(
    aa = mean(threat_AA, na.rm = TRUE),
    bru = mean(threat_BRU, na.rm = TRUE),
    rcd = mean(threat_RCD, na.rm = TRUE),
    isgd = mean(threat_ISGD, na.rm = TRUE),
    epm = mean(threat_EPM, na.rm = TRUE),
    cc = mean(threat_CC, na.rm = TRUE),
    hid = mean(threat_HID, na.rm = TRUE),
    p = mean(threat_P, na.rm = TRUE),
    ts = mean(threat_TS, na.rm = TRUE),
    nsm = mean(threat_NSM, na.rm = TRUE),
    ge = mean(threat_GE, na.rm = TRUE),
    na = mean(threat_NA, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(aa, bru, rcd, isgd, epm, cc, hid, p, ts, nsm, ge, na),
               names_to = 'threat', values_to = 'pct_species') %>% 
  mutate(c_pct_species = 1 - pct_species) %>% 
  pivot_longer(cols = c(pct_species, c_pct_species)) %>% 
  ungroup()

threat_palette <- c("#d8d3cd", "#158467")

group_threats %>% 
  ggplot() + 
  geom_col(aes(x = "", y = value, group = name, fill = name),
           size = 1, color = NA) + 
  scale_fill_manual(values = threat_palette) + 
  guides(fill = FALSE) + 
  facet_grid(rows = vars(group), cols = vars(threat)) +
  coord_polar('y') + 
  theme_void() + 
  theme(
    strip.background = element_blank()
  )
