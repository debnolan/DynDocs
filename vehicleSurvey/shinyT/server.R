
library(shiny)
y = c(-4, 3, 7, 5)
bootSamp = expand.grid(y,y,y,y)
bootSamp$mean = apply(as.matrix(bootSamp), 1, mean)
bootMean = bootSamp$mean


shinyServer(function(input, output) {
    
    output$summary <- renderTable({
      data.frame(table(bootMean))
    })
    
    output$table <- renderTable({ 
      bootSamp[input$obs[1]:input$obs[2], ]
     
    })
    
    output$plot <- renderPlot({ 
      x = bootMean
      bins = seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, main = "Exact Bootstrap Sampling Distribution", 
           xlab = "mean", col = 'turquoise', border = 'white')
    })
    
  }
)