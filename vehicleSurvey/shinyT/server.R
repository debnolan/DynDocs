
library(shiny)
y = c(-4, 3, 7, 5)
bootSamp = expand.grid(y,y,y,y)
bootSamp$mean = apply(as.matrix(bootSamp), 1, mean)
bootMean = bootSamp$mean


shinyServer(function(input, output) {
    
    output$summary <- renderPrint({
      table(as.factor(bootMean))
    })
    
    output$table <- renderTable({ 
      head(bootSamp, input$obs)
    })
    
    output$plot <- renderPlot({ 
      hist(bootMean, breaks = input$bins)
    })
    
  }
)