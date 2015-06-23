library("shiny")
source("helpers.R")

shinyServer(
  function(input, output) {
    output$data <- renderTable({
      data.frame(calcium = c(calcium, NA), placebo = placebo)
    })
    output$compare <- renderTable({
      data.frame(calcium = mean(calcium), placebo = mean(placebo))
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
    
    output$sum <- renderTable({
      data.frame(calcium = sum(dichData(calcium, placebo, thresh = input$v)$dich1, 
                               na.rm = TRUE),
                placebo = sum(dichData(calcium, placebo, thresh = input$v)$dich2))
    })
    
    output$dich <- renderPlot({
      args <- list(NRep = input$r, thresh = input$v,
                   pop1 = calcium, pop2 = placebo)
      do.call(dichPlot, args)
    })
    
    output$rank <- renderTable({
      rankData(calcium, placebo)
    })
    
    output$wilcox <- renderPlot({
      args <- list(NRep = input$p,
                   pop1 = calcium, pop2 = placebo)
      do.call(wilPlot, args)
    })
  }
)
