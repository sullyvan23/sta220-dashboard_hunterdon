# app.R
# A base dashboard to edit for your final project. See "Using the Dashboard Template."
# Based on "Hello Shiny!" https://shiny.posit.co/r/gallery/start-simple/faithful/

library(shiny)
library(tidyverse)


# The Data ---------------------------------------------------------------------------------------

# Save your cleaned data in app/data/, then delete the # below.
# Yes, you will have cleaned data in two places in your repo.
# my_data <- read_csv("data/my_data.csv")


# The UI (what the reader sees) ------------------------------------------------------------------

ui <- fluidPage(

  titlePanel("your title"),
  p("For: who is making the decision"),
  p("The decision: the one decision this dashboard helps with"),

  selectInput(inputId = "pick", label = "Pick one:", choices = c("choice 1", "choice 2")),

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

    ggplot()     # replace with your code for Figure 1

  })

  output$fig_2 <- renderPlot({

    ggplot()     # replace with your code for Figure 2

  })

}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
