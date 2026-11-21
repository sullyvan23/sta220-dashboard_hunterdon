# app.R
# A base dashboard to edit for your final project. See "Using the Dashboard Template."
# Based on "Hello Shiny!" https://shiny.posit.co/r/gallery/start-simple/faithful/

library(shiny)
library(tidyverse)
library(sf)
library(ggrepel)

# The Data ---------------------------------------------------------------------------------------

incidents_by_grade <- read_csv("data/incidents_by_grade.csv")
ela_perf_by_grade <- read_csv("data/ela_perf_by_grade.csv")
math_perf_by_grade <- read_csv("data/math_perf_by_grade.csv")
st_ratio_by_grade <- read_csv("data/st_ratio_by_grade.csv")
per_pup_cst_by_grade <- read_csv("data/per_pup_cst_by_grade.csv")
teach_exp_by_grade <- read_csv("data/teach_exp_by_grade.csv")
teach_adv_degree_by_grade <- read_csv("data/teach_adv_degree_by_grade.csv")

hunterdon_towns_geom <- readRDS("data/hunterdon_towns_geom.rds")


metrics_choices <- list("Incidents" = incidents_by_grade,
                "ELA Testing Performance" = ela_perf_by_grade,
                "Math Testing Performance" = math_perf_by_grade,
                "Student-Teacher Ratio" = st_ratio_by_grade,
                "Per Pupil Expenditures" = per_pup_cst_by_grade,
                "Avg Teacher Experience" = teach_exp_by_grade,
                "Teacher Advanced Degree %" = teach_adv_degree_by_grade)

grades_choices <- c("K or Younger" = 0, 
            "1st" = 1, 
            "2nd" = 2, 
            "3rd" = 3, 
            "4th" = 4, 
            "5th" = 5, 
            "6th" = 6, 
            "7th" = 7, 
            "8th" = 8, 
            "9th" = 9, 
            "10th" = 10, 
            "11th" = 11, 
            "12th" = 12)


# The UI (what the reader sees) ------------------------------------------------------------------

ui <- fluidPage(

  titlePanel("Hunterdon Towns Children Metrics"),
  p("For: Parents or prospective-parents of children ages K-12"),
  p("The decision: Find the best towns in Hunterdon for your child to grow up in"),

  ### inputs, need the uiOutput part to get the dynamically created kids inputs
  selectInput(inputId = "metric", label = "Metric:", choices = names(metrics_choices)),
  selectInput(inputId = "kids", label = "# of Kids:", choices = 1:8),
  selectInput(inputId = "label_town", label = "Label Towns?", choices = c("Yes", "No")),
  uiOutput("dynamic_kids_inputs"),
  
  
  h3("Figure 1. Individual Metrics by Town and Grade"),
  plotOutput(outputId = "fig_1"),
  p(HTML("This figure colors every town in Hunterdon County from red (bad) to green (good) based on
    the chosen metric. Average is taken by grade, based on what schools they will go to in 
    each future grade in that town.<br>
    NOTE: Readington has two different elementary schools for grades K-3rd. Numbers provided are
          an average of both schools.<br>
          Flemington/Raritan has four different elementary schools for grades K-4th. Numbers 
          provided are an average of all four schools")),

  h3("Figure 2. "),
  plotOutput(outputId = "fig_2"),
  p(""),

  h3("What this dashboard cannot tell you"),
  p("This data is limited to school data (and potentially more like crime and safety). This data
    should be used supplementary to other factors involved in deciding where to live such as 
    cost of living, proximity to work, etc.")

)


# The Server (how the figures are made) ----------------------------------------------------------

server <- function(input, output) {
  
  ### creates n kids inputs, depending on previous # of kids input
  output$dynamic_kids_inputs <- renderUI({
    
    ### apply from 1 to n kids
    lapply(
      1:input$kids, function(i) {
      
      ### ask what grade kid n is going into
      selectInput(inputId = paste0("kid_", i, "_grade"),
                  label = paste0("Kid ", i, " Going Into Grade:"),
                  choices = names(grades_choices))
      
      }
    )
      
  })
  

  output$fig_1 <- renderPlot({
    
    ### getting dataset to use
    selected_data <- metrics_choices[[input$metric]]
    
    ### just to make sure key matches
    selected_data$GEOID <- as.character(selected_data$GEOID)
    hunterdon_towns_geom$GEOID <- as.character(hunterdon_towns_geom$GEOID)
    
    ### start off with 0 kids going to go through each grade
    ### organized like this just to keep track of numbers (K, 1-5, 6-8, 9-12)
    kids_grades <- c(0,
                     0,0,0,0,0,
                     0,0,0,
                     0,0,0,0)
    
    ### Adding 1 to grade if a kid is going into that grade or younger (will eventually get there)
    for(kid_n in 1:input$kids) {
      for(grade in 0:12) {
        
        ### if grade >= the grade the kid is going into
        if( grade >= grades_choices[[ input[[paste0("kid_", kid_n, "_grade")]] ]] ) {
          kids_grades[grade+1] <- kids_grades[grade+1] + 1
        }
        
      }
    }
    
    
    ### getting average based off grade chosen
    ### weight by how many kids will go through each grade
    ### 2 applies the * by column
    ### then divide by sum of weights (where there aren't NAs)
    selected_data$Average <- 
      rowSums(sweep(as.matrix(selected_data %>% select(kindergarten:twelfth)),
                    2, kids_grades, "*"), 
                        na.rm = TRUE) / 
      rowSums(sweep(as.matrix(!is.na(selected_data %>% select(kindergarten:twelfth))),
                    2, kids_grades, "*"))
                               
    ### joining to geom dataset
    metric_geom <- hunterdon_towns_geom %>%
      left_join(selected_data %>% select(GEOID, Average),
                join_by(GEOID))
    
    ### base plot
    plot <- ggplot(metric_geom) +
            geom_sf(aes(fill = Average)) +
            theme_void()
    
    
    ### putting town labels if wanted
    if(input$label_town == "Yes") {
      plot <- plot +
        geom_text_repel(aes(geometry = geometry, label = NAME),
                        seed = 111, stat = "sf_coordinates",
                        size = 3, fontface = "bold",
                        box.padding = 0.025)
    }
    
    
    ### changing if red->green or green->red
    if(input$metric %in% c("Incidents", "Student-Teacher Ratio")) {
      plot <- plot +
          scale_fill_gradient2(low = "green", mid = "white", high = "red",
                             midpoint = median(metric_geom$Average, na.rm = TRUE))
    } else {
      plot <- plot +
        scale_fill_gradient2(low = "red", mid = "white", high = "green",
                             midpoint = median(metric_geom$Average, na.rm = TRUE))
    }
    
    
    ### adding needed titles, subtitles, and captions based on metric
    if(input$metric == "Incidents") {
      plot <- plot +
        labs(title = "2024-25 Incidents Reported Per 100 Students",
             subtitle = "Incidents of Violence, Weapons, Vandalism, Substances, or Bullying",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)")
      
    } else if(input$metric == "ELA Testing Performance") {
      plot <- plot +
        labs(title = "2024-25 Percent of Students Meeting Federal ELA Proficiency Standards",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)
                        NOTE: No data available for Patrick McGaheran School (K-2nd for 
                              Clinton Twp). Average taken removing NAs.")
      
    } else if(input$metric == "Math Testing Performance") {
      plot <- plot +
        labs(title = "2024-25 Percent of Students Meeting Federal Math Proficiency Standards",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)
                        NOTE: No data available for Patrick McGaheran School (K-2nd for 
                              Clinton Twp). Average taken removing NAs.")
      
    } else if(input$metric == "Student-Teacher Ratio") {
      plot <- plot +
        labs(title = "2024-25 Students Per 1 Teacher",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)")
      
    } else if(input$metric == "Per Pupil Expenditures") {
      plot <- plot +
        labs(title = "2024-25 School Expenditures (in $) Per Student",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)")
      
    } else if(input$metric == "Avg Teacher Experience") {
      plot <- plot +
        labs(title = "2024-25 Average Years of Experience for Teachers",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)")
      
    } else if(input$metric == "Teacher Advanced Degree %") {
      plot <- plot +
        labs(title = "2024-25 Percent of Teachers with a Masters or Better Degree",
             subtitle = "",
             caption = "SOURCE: NJ DOE (2024-25 School Performance Reports)")
      
    } 
    
    
    plot
    
  })

  
  output$fig_2 <- renderPlot({

    

  })

}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
