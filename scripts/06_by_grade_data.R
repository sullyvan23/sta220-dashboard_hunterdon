
### getting incidents per 100 students for every grade in every town (school they go to)
ela_perf_by_grade <- by_grade_join(hunterdon_ela_performance_24_25, federalproficiencyrate_school)

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

