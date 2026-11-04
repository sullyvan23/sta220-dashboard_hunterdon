
### only want to look at town / boro numbers (filter out high schools)
hunterdon_twn_pupcst_24_25 <- hunterdon_pupcst_24_25 %>% filter(!district_name %in%
                                                                c("South Hunterdon Regional School District",
                                                                  "North Hunterdon-Voorhees Regional",
                                                                  "Hunterdon Central Reg",
                                                                  "Delaware Valley Regional",
                                                                  "Hunterdon Co Vocational"))


ggplot(hunterdon_twn_pupcst_24_25, aes(x = total_budgetary_comparative_per_pupil_cost, 
                                       y = reorder(district_name, total_budgetary_comparative_per_pupil_cost))) +
      geom_col() +
      labs(title = "Which Hunterdon School Districts Spend \nthe Most Per Pupil?",
           subtitle = "Each bar is a township/borough school district in Hunterdon County",
           x = "2024-25 Budget Per Pupil (in $)",
           y = "Township / Borough",
           caption = "Source: NJ DOE (2026-27 User-Friendly Budgets)") +
      theme_minimal()

      