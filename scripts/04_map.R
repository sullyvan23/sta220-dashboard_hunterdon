install.packages("sf")

library(sf)
library(tidyverse)

### https://www.rdocumentation.org/packages/tigris/versions/2.2.1


### getting Hunterdon town boundaries
### Take out Bloombsbury because it doesn't feed into the 5 Hunterdon hugh schools like others
hunterdon_towns <- st_read("data/Hunterdon_Municipal_Boundaries/NJ_Municipal_Boundaries_3424.shp") %>%
                   filter(COUNTY == "HUNTERDON",
                          NAME != "Bloomsbury Borough")

ggplot(hunterdon_towns) +
  geom_sf() +
  geom_sf_text(aes(label = NAME), size = 2)


######################################################################################################
install.packages("tigris")
library(tigris)

### gathering school district geometry
hunterdon_elem_districts <- school_districts(state = "NJ",
                                             type = "elementary",
                                             year = 2025,
                                             cb = TRUE)

south_hunterdon_district <- school_districts(state = "NJ",
                                             type = "secondary",
                                             year = 2025,
                                             cb = TRUE)


### roads to potentially use for more splitting
hunterdon_roads <- roads("New Jersey", "Hunterdon")

### filtering for only Hunterdon districts
hunterdon_elem_districts <- hunterdon_elem_districts %>% 
                            filter(NAME %in% c(hunterdon_ela_performance_24_25$districtname,
                                               "Clinton Town-Glen Gardner School District"))

### filtering out different Franklin Township School Districts
hunterdon_elem_districts <- hunterdon_elem_districts %>% 
                            filter(!ELSDLEA %in% c("05430", "05460"))

ggplot(hunterdon_elem_districts) +
  geom_sf() +
  geom_sf_text(aes(label = ELSDLEA), size = 2.5)
