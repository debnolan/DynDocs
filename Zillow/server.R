# server.R

library(lattice)
library(mosaic)
library(ggplot2)
source("helper.R")

shinyServer(function(input, output) {

  plotParams <- reactiveValues(plotT = NULL, varN = NULL)

  observeEvent( input$makePlot, {
      plotParams$plotT = input$plotType
      plotParams$varN = input$varName
    })
  
  output$plotA <- renderPlot({  
    choosePlot(plotParams$varN, plotParams$plotT , showing = "All")
  })
})
