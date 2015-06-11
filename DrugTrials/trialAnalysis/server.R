library("shiny")
source("helpers.R")

shinyServer(
  function(input, output) {
    output$data <- renderTable({
      data.frame(calcium = c(calcium, NA), placebo = placebo)
    })
    output$compare <- renderPrint({
      c(mean(calcium), mean(placebo))
    })
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
    
    output$dataDich <- renderTable({
      dichData(calcium, placebo, thresh = input$v)
    })
    
    output$dich <- renderPlot({
      args <- list(NRep = input$r, thresh = input$v,
                   pop1 = calcium, pop2 = placebo)
      do.call(dichPlot, args)
    })
    
    output$rank <- renderTable({
      rankData(calcium, placebo, threshold = input$t)
    })
    
    output$wilcox <- renderPlot({
      args <- list(NRep = input$p, comp = input$t,
                   pop1 = calcium, pop2 = placebo)
      do.call(wilPlot, args)
    })
  }
)
