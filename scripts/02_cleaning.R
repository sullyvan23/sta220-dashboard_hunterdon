
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



### getting a numeric students per teachers instead of "#:01"
### using this method works for no more than 100 students per teacher, which was all schools in Hunterdon
hunterdon_ratios_24_25 <- hunterdon_ratios_24_25 %>%
                mutate(students_per_teacher = substr(student_teacher_school, 1, 2),
                       students_per_teacher = as.numeric(gsub(":", "", students_per_teacher)))


### uisng only all students, not by any subgroups, and making columns numeric
hunterdon_ela_performance_24_25 <- hunterdon_ela_performance_24_25 %>%
              filter(studentgroup == "All Students") %>% 
              mutate(across(participationrate_school:federalproficiencyrate_school, ~
                            as.numeric(gsub("%", "", .x))))

hunterdon_math_performance_24_25 <- hunterdon_math_performance_24_25 %>%
              filter(studentgroup == "All Students") %>% 
              mutate(across(participationrate_school:federalproficiencyrate_school, ~
                              as.numeric(gsub("%", "", .x))))


### changing GIOID to character, must've made it a number during editing in excel
town_schools_by_grade$GEOID <- as.character(town_schools_by_grade$GEOID)


### making Hunterdon students per teacher
hunterdon_stud_per_teach_24_25 <- hunterdon_teacher_exp_24_25 %>%
                                select(schoolname:teachercount_school) %>%
                                left_join(hunterdon_enrollment_24_25 %>%
                                              select(schoolname, total),
                                          join_by(schoolname)) %>%
                                mutate(stud_per_teach = total / teachercount_school)


### issue with \n instead of space
hunterdon_school_expenditures_24_25 <- hunterdon_school_expenditures_24_25 %>%
                          mutate(schoolname = gsub("\n", " ", schoolname))

### making per pupil costs a number, making numeric and NAs 0
hunterdon_school_expenditures_24_25 <- hunterdon_school_expenditures_24_25 %>%
                        filter(!schoolname %in% c("District Level Total Expenditures")) %>%
                        mutate(across(federal_per_pupcst:total_per_pupcst, ~
                                        as.numeric(
                                          gsub("\\$", "", 
                                                      gsub(",", "", .x)) ) )) %>%
                        mutate(across(federal_per_pupcst:total_per_pupcst, ~
                                        replace_na(.x, 0)))

### adding districts central office numbers to all schools
hunterdon_school_expenditures_24_25 <- hunterdon_school_expenditures_24_25 %>%
                        group_by(district_id) %>%
                        mutate(across(federal_per_pupcst:total_per_pupcst, ~
                                      .x + .x[schoolname == "District-Central Office"])) %>%
                        ungroup() %>%
                        filter(schoolname != "District-Central Office")


### making teacher education actually numeric
hunterdon_teacher_edu_24_25 <- hunterdon_teacher_edu_24_25 %>%
                               filter(`teachers/admins` == "Teachers") %>% 
                              mutate(across(bachelors:doctoral, ~
                                              as.numeric(gsub("%", "", .x))),
                                     degree = bachelors + masters + doctoral,
                                     advanced_degree = masters + doctoral)


  