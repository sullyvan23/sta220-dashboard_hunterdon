
### removing weird character in a lot of the names
town_schools_by_grade <- town_schools_by_grade %>% 
              mutate(across(kindergarten:twelfth, ~ gsub("\u00A0", " ", .x)))



### function to get variable by grade for each town
by_grade_join <- function(data_joining, variable) {
  
  ### take school names and variable I want, making sure numeric
  data_joining <- data_joining %>%
                  select(schoolname, {{variable}})
  
  ### pivot my towns by grade dataset so I do 1 join instead of 1 for every grade
  new_dataset <- town_schools_by_grade %>% 
                pivot_longer(kindergarten:twelfth,
                             names_to = "grade",
                             values_to = "schoolname")
  
  ### taking out name discrepancies (District)
  data_joining$schoolname <- gsub(" District", "", data_joining$schoolname)
  
  ### left join to pivot longer school by grade
  new_dataset <- new_dataset %>%
                left_join(data_joining, join_by(schoolname))
  
  ### pivot wider to go back to normal, take mean for when multiple elementary schools in district
  new_dataset <- new_dataset %>%
                select(-schoolname) %>%
                pivot_wider(names_from = grade,
                            values_from = {{variable}},
                            values_fn = mean)
  
  ### return dataset
  return(new_dataset)
}

################################################################################################################

### getting data for every grade in every town (school they go to)
incidents_by_grade <- by_grade_join(hunterdon_incidents_24_25, incidentsper100students)
### no Patrick McGaheran for ELA and math
ela_perf_by_grade <- by_grade_join(hunterdon_ela_performance_24_25, federalproficiencyrate_school)
math_perf_by_grade <- by_grade_join(hunterdon_ela_performance_24_25, federalproficiencyrate_school)
st_ratio_by_grade <- by_grade_join(hunterdon_ratios_24_25, students_per_teacher)






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

################################################################################################################

### getting incidents per 100 students for every grade in every town (school they go to)


### get different number depending on grade going into (0-12, columns 5-17)
going_into_grade <- 0

### creating column of average based on grade going into
ela_perf_by_grade$avg_incidents_per100 <- 
  rowMeans(ela_perf_by_grade[,(5+going_into_grade):17], na.rm = TRUE)


### join to town geometry dataset
ela_perf_geom <- hunterdon_towns_geom %>%
  left_join(ela_perf_by_grade %>% select(GEOID, federalproficiencyrate_school),
            join_by(GEOID))

################################################################################################################

