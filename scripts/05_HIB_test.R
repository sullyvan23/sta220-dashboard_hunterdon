
### hunterdon_incidents_24_25 %>% select(schoolname, incidentsper100students)
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


incidents_by_grade <- by_grade_join(hunterdon_incidents_24_25, incidentsper100students)


incidents_geom <- hunterdon_towns_geom %>%
                  left_join(incidents_by_grade %>% select(GEOID, kindergarten:twelfth),
                            join_by(GEOID))
going_into_grade <- 0




