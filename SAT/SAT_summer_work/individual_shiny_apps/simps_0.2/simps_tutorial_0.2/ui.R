library(shiny)

# Define UI for slider demo application
  shinyUI(fluidPage(
    #  Application title
    titlePanel("Simpson's Paradox"),
    # options
    sidebarLayout(
      sidebarPanel(
        # option to fix the conditional proabilities
        sliderInput("success_rate_A_small2", "Treatment A Success Rate For Small Kidney Stones:",
                    min = 0, max = 1, value = 0.93, step = 0.01),
        sliderInput("success_rate_B_small2", "Treatment B Success Rate for Small Kidney Stones:",
                    min = 0, max = 1, value = 0.87, step = 0.01),
        sliderInput("success_rate_A_large2", "Treatment A Success Rate For Large Kidney Stones:",
                    min = 0, max = 1, value = 0.73, step = 0.01),
        sliderInput("success_rate_B_large2", "Treatment B Success Rate for Large Kidney Stones:",
                    min = 0, max = 1, value = 0.69, step = 0.01),
        sliderInput("count_A_small2", "Treatment A Count For Small Kidney Stones:",
                    min = 0, max = 350, value = 87, step = 1),
        sliderInput("count_B_small2", "Treatment B Count For Small Kidney Stones:",
                    min = 0, max = 350, value = 263, step = 1)
        ),
        # Show a table summarizing the values entered
        mainPanel(
          tableOutput("table2"),
          textOutput("yesorno2")
      )
    )
  ))