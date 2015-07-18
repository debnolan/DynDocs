library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(
  #  Application title
  titlePanel("Sliders"),
  # options
  sidebarLayout(
    sidebarPanel(
      # option to fix the conditional proabilities
      sliderInput("success_rate_A_small", "Treatment A Success Rate For Small Kidney Stones:",
                  min = 0, max = 1, value = 0.93, step = 0.01),
      sliderInput("success_rate_B_small", "Treatment B Success Rate for Small Kidney Stones:",
                  min = 0, max = 1, value = 0.87, step = 0.01),
      sliderInput("success_rate_A_large", "Treatment A Success Rate For Large Kidney Stones:",
                  min = 0, max = 1, value = 0.73, step = 0.01),
      sliderInput("success_rate_B_large", "Treatment B Success Rate for Large Kidney Stones:",
                  min = 0, max = 1, value = 0.69, step = 0.01),
#       # option to fix the total procedure counts
#       sliderInput("Treatment_count_A", "Treatment A Total Counts:",
#                   min = 0, max = 1, value = 0.87),
#       sliderInput("Treatment_count_B", "Treatment B Total Counts:",
#                   min = 0, max = 1, value = 0.87),
      sliderInput("count_A_small", "Treatment A Count For Small Kidney Stones:",
                  min = 0, max = 350, value = 87, step = 1),
      sliderInput("count_B_small", "Treatment B Count For Small Kidney Stones:",
                  min = 0, max = 350, value = 263, step = 1)
      ),
      # Show a table summarizing the values entered
      mainPanel(
        tableOutput("table")
    )
  )
))