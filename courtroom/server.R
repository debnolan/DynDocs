library(shiny)

source("helpers.R")

shinyServer(function(input, output) {
  
  # Computes table of observed values from from client input
  counts = reactive({
    
    # Determines which of the four functions to use from helpers.R
    tabGen = switch(input$whichCell,
                    cell11 = tabGen11,
                    cell21 = tabGen21,
                    cell12 = tabGen12,
                    cell22 = tabGen22,
                    tabGen11)
    tabGen({
      if (is.null(input$cellCount)) {
        40
      } else {
        input$cellCount
      }
    })
  })
  
  # Generates plots and summary of test for display output
  output$chiSqPlot = renderPlot(plotGen(counts()))
  output$residPlot = renderPlot(residGen(counts()))
  output$summary = renderText(conclusion(counts()))
  
})