# app.R
# A base dashboard to edit for your final project. See "Using the Dashboard Template."
# Based on "Hello Shiny!" https://shiny.posit.co/r/gallery/start-simple/faithful/

library(shiny)
library(tidyverse)


# The Data ---------------------------------------------------------------------------------------

incidents_by_grade <- read_csv("data/incidents_by_grade.csv")
ela_perf_by_grade <- read_csv("data/ela_perf_by_grade.csv")
math_perf_by_grade <- read_csv("data/math_perf_by_grade.csv")
st_ratio_by_grade <- read_csv("data/st_ratio_by_grade.csv")
per_pup_cst_by_grade <- read_csv("data/per_pup_cst_by_grade.csv")
teach_exp_by_grade <- read_csv("data/teach_exp_by_grade.csv")
teach_adv_degree_by_grade <- read_csv("data/teach_adv_degree_by_grade.csv")

hunterdon_towns_geom <- readRDS("data/hunterdon_towns_geom.rds")


metrics <- list("Incidents" = incidents_by_grade,
                "ELA Testing Performance" = ela_perf_by_grade,
                "Math Testing Performance" = math_perf_by_grade,
                "Student-Teacher Ratio" = st_ratio_by_grade,
                "Per Pupil Expenditures" = per_pup_cst_by_grade,
                "Avg Teacher Experience" = teach_exp_by_grade,
                "Teacher Advanced Degree %" = teach_adv_degree_by_grade)

grades <- c("K" = 0, 
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

  titlePanel("your title"),
  p("For: who is making the decision"),
  p("The decision: the one decision this dashboard helps with"),

  
  selectInput(inputId = "metric", label = "Metric:", choices = names(metrics)),
  selectInput(inputId = "grade", label = "Going Into Grade:", choices = names(grades)),
  
  
  h3("Figure 1. title"),
  plotOutput(outputId = "fig_1"),
  p("a sentence or two saying what the figure shows."),

  h3("Figure 2. title"),
  plotOutput(outputId = "fig_2"),
  p("a sentence or two saying what the figure shows."),

  h3("What this dashboard cannot tell you"),
  p("the limits of your data.")

)


# The Server (how the figures are made) ----------------------------------------------------------

server <- function(input, output) {

  output$fig_1 <- renderPlot({
    
    ### getting dataset to use
    selected_data <- metrics[[input$metric]]
    
    ### getting average based off grade chosen
    selected_data$avg_used <- 
      rowMeans(selected_data[,(5+grades[[input$grade]]):17], na.rm = TRUE)
    
    selected_data$GEOID <- as.character(selected_data$GEOID)
                               
    ### joining to geom dataset
    metric_geom <- hunterdon_towns_geom %>%
      left_join(selected_data %>% select(GEOID, avg_used),
                join_by(GEOID))
    
    ### have to be able to change if high or low is green depending on metric
    plot <- ggplot(metric_geom) +
            geom_sf(aes(fill = avg_used)) +
            theme_minimal()
    
    ### changing if red->green or green->red
    if(input$metric %in% c("Incidents", "Student-Teacher Ratio")) {
      plot <- plot +
          scale_fill_gradient2(low = "green", mid = "white", high = "red",
                             midpoint = median(metric_geom$avg_used, na.rm = TRUE))
    } else {
      plot <- plot +
        scale_fill_gradient2(low = "red", mid = "white", high = "green",
                             midpoint = median(metric_geom$avg_used, na.rm = TRUE))
    }
    
    
    plot
    
  })

  
  output$fig_2 <- renderPlot({

    

  })

}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
