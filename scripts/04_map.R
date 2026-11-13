install.packages("sf")
install.packages("tigris")

library(sf)
library(tigris)
library(tidyverse)

### https://www.rdocumentation.org/packages/tigris/versions/2.2.1

### gathering Hunterdon towns geometry
hunterdon_towns_geom <- county_subdivisions(state = "NJ",
                                            county = "Hunterdon",
                                            year = 2025,
                                            cb = TRUE)


### filtering out Bloomsbury as only town that doesn't go to one of 5 main high schools
hunterdon_towns_geom <- hunterdon_towns_geom %>% 
                        filter(NAME != "Bloomsbury")

### plot check
ggplot(hunterdon_towns_geom) +
  geom_sf() +
  geom_sf_text(aes(label = NAMELSAD), size = 2)
