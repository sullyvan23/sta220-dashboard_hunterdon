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

### differentiating Clinton Town and Township
hunterdon_towns_geom <- hunterdon_towns_geom %>%
                        mutate(NAME = ifelse(NAMELSAD == "Clinton town",
                                             "Clinton Town",
                                             NAME),
                               NAME = ifelse(NAMELSAD == "Clinton township",
                                             "Clinton Twp",
                                             NAME))

### plot check
ggplot(hunterdon_towns_geom) +
  geom_sf() +
  geom_sf_text(aes(label = NAME), size = 2)
