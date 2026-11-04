
dim(pupcst)
names(pupcst)
summary(pupcst)

### columns year_1 to year_5 are characters when should be number
### so taking the last audited cost column, and renaming it as so, and making numeric
pupcst_24_25 <- pupcst %>% select(-c(year_1, year_3:year_5)) %>%
                           rename(pupcst_24_25 = year_2) %>%
                           mutate(pupcst_24_25 = as.numeric(pupcst_24_25))

summary(pupcst_24_25)

### pivoting so different cost types are their own columns for each district
pupcst_24_25 <- pupcst_24_25 %>% mutate(line_desc = gsub(" ", "_", tolower(line_desc)),
                                        line_desc = gsub("-", "_", line_desc)) %>%
                                 group_by(district_id) %>%
                                 pivot_wider(names_from = line_desc,
                                             values_from = pupcst_24_25)

### filtering to only Hunterdon County districts
hunterdon_pupcst_24_25 <- pupcst_24_25 %>% filter(county_name == "Hunterdon")

