# server.R
library (shiny)
library(lattice)
library(mosaic)
library(ggplot2)
source("helper.R")


shinyServer(function(input, output) {
  
  plotParams <- reactiveValues(plotT = NULL, varN = NULL, show = NULL, var = NULL, show1 = NULL)
  
  observeEvent(input$makePlot, {
    plotParams$plotT = input$plotType
    plotParams$varN = input$variable
    plotParams$show = input$showing
  })
  
  output$plotA <- renderPlot({  
    choosePlot(plotParams$varN, plotParams$plotT , plotParams$show)
  })
 
  observeEvent(input$makePlot2, {
    plotParams$var = input$varName
    plotParams$show1 = input$showing1
  })
  
  output$plotB <- renderPlot({  
    RegreF(plotParams$var, plotParams$show1)
  })
})