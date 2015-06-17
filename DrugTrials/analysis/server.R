library("shiny")
source("helpers.R")

shinyServer(
  function(input, output) {
    dataSet <- reactive({ 
      drugData(input$var)
      })
      output$data <- renderTable({
        data.frame(dataSet())
      })
    output$mean <- renderTable({
      data.frame("drug" = mean(dataSet()[,1], na.rm = TRUE), 
                 "placebo" = mean(dataSet()[,2]))
    })
    output$permPlot <- renderPlot({
      args <- list(NRep = input$NRep1, comp = input$thresh,
                   pop1 = dataSet()[1:10,1], pop2 = dataSet()[,2])
      do.call(plotPerm, args)
    })
  output$plot <- renderPlot({
  args <- list(min = input$range[1], max = input$range[2], comp = input$thresh,
                   pop1 = dataSet()[1:10,1], pop2 = dataSet()[,2])
      
      do.call(pValPlot, args)
    })
  
  output$dataDich <- renderTable({
    dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2)
  })
  
  output$sum <- renderTable({
    data.frame(drug = sum(dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2)$dich1, 
                             na.rm = TRUE),
               placebo = sum(dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2)$dich2))
  })
  
  output$dichPlot <- renderPlot({
    args <- list(NRep = input$NRep2, thresh = input$thresh2,
                 pop1 = dataSet()[1:10,1], pop2 = dataSet()[,2])
    do.call(dichPlot, args)
  })
  
  output$rank <- renderTable({
    rankData(dataSet()[,1], dataSet()[,2])
  })
  
  output$rankSum <- renderText({
    paste("Ranked Sum =", 
          sum(
            rankData(
            pop1 = dataSet()[,1], pop2 = dataSet()[,2]
                       )$rankedDrug, na.rm = TRUE)
    )
  })
  
  output$wilcox <- renderPlot({
    args <- list(NRep = input$NRep3,
                 pop1 = dataSet()[1:10,1], pop2 = dataSet()[,2])
    do.call(wilPlot, args)
  })
  }
)