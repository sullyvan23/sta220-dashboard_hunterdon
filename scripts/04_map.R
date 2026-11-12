install.packages("tigris")
install.packages("sf")

library(tigris)
library(sf)
library(tidyverse)

### https://www.rdocumentation.org/packages/tigris/versions/2.2.1

### gathering school district geometry
hunterdon_elem_districts <- school_districts(state = "NJ",
                                             type = "elementary",
                                             year = 2025,
                                             cb = TRUE)

### filtering for only Hunterdon districts
hunterdon_elem_districts <- hunterdon_elem_districts %>% 
                            filter(NAME %in% c(hunterdon_ela_performance_24_25$districtname,
                                               "Clinton Town-Glen Gardner School District"))

### filtering out a different Franklin Township School District
hunterdon_elem_districts <- hunterdon_elem_districts %>% 
                            filter(ELSDLEA != "05430")

ggplot(hunterdon_elem_districts) +
  geom_sf() +
  geom_sf_text(aes(label = ELSDLEA), size = 2.5)
