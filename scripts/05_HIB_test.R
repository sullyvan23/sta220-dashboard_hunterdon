
### function to get variable by grade for each town
by_grade_join <- function(data_joining, variable) {
  
  data_joining <- data_joining %>%
                  select(schoolname, {{variable}})
  
  new_dataset <- town_schools_by_grade %>% 
                 pivot_longer(kindergarten:twelfth,
                              names_to = "grade",
                              values_to = "schoolname")
  
  
  new_dataset$schoolname <- gsub("\u00A0", " ", new_dataset$schoolname)
  new_dataset$schoolname <- gsub(" District", "", new_dataset$schoolname)
  
  data_joining$schoolname <- gsub("\u00A0", " ", data_joining$schoolname)
  data_joining$schoolname <- gsub(" District", "", data_joining$schoolname)
  
  new_dataset <- new_dataset %>%
                 left_join(data_joining, join_by(schoolname))
  
  
  new_dataset <- new_dataset %>%
                 select(-schoolname) %>%
                 pivot_wider(names_from = grade,
                             values_from = {{variable}},
                             values_fn = mean)
    
  return(new_dataset)
}

### getting incidents per 100 students for every grade in every town (school they go to)
incidents_by_grade <- by_grade_join(hunterdon_incidents_24_25, incidentsper100students)

### changing GIOID to character, must've made it a number during editing in excel
incidents_by_grade$GEOID <- as.character(incidents_by_grade$GEOID)

### get different number depending on grade going into (0-12, columns 5-17)
going_into_grade <- 0

### creating column of average based on grade going into
incidents_by_grade$avg_incidents_per100 <- 
  rowMeans(incidents_by_grade[,(5+going_into_grade):17], na.rm = TRUE)


### join to town geometry dataset
incidents_geom <- hunterdon_towns_geom %>%
                  left_join(incidents_by_grade %>% select(GEOID, avg_incidents_per100),
                            join_by(GEOID))

### plot
ggplot(incidents_geom) +
  geom_sf(aes(fill = avg_incidents_per100)) +
  scale_fill_gradient2(low = "green", mid = "white", high = "red",
                       midpoint = mean(incidents_geom$avg_incidents_per100)) +
  theme_minimal()




