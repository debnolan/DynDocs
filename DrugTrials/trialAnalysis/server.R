library("shiny")
source("helpers.R")

shinyServer(
  function(input, output) {
    output$perm <- renderPlot({
    args <- list(NRep = input$n, comp = input$k,
                   pop1 = calcium, pop2 = placebo)
    do.call(plotPerm, args)
    })
    
    
    output$plot <- renderPlot({
      args <- list(min = input$range[1], max = input$range[2], 
                   pop1 = calcium, pop2 = placebo)
      
      do.call(pValPlot, args)
    })
  }
)
