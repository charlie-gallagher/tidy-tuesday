library(tidyverse)
library(sf)
library(censusapi)
library(tigris)
Sys.setenv(CENSUS_KEY="cfc45d3a35366fdd22545ff79b97a714bdac44a4")


censusapi::listCensusMetadata(name = 'acs/acs5/profile', vintage = 2017, type = 'variables') %>%
  View()

pop_csa <- censusapi::getCensus(name = 'acs/acs5/profile', vintage = 2017, 
                     region = 'combined statistical area:*',
                     vars = 'DP05_0033E')

names(pop_csa) <- c('csa','pop')

csa <- combined_statistical_areas()
csa <- st_as_sf(csa)
csa <- merge(csa, pop_csa, by.x = 'CSAFP', by.y = 'csa')

csa <- csa %>%
  filter(as.numeric(INTPTLON) > -120, as.numeric(INTPTLAT) > 25)



state <- tigris::states()
state <- st_as_sf(state) %>%
  rownames_to_column() %>%
  filter(!rowname %in% c(31, 34, 35, 36, 40, 41, 49))


save(csa, state, file = 'data/csa.RData')
