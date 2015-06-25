library(shiny)
library(survey)
source("helpers.R")

shinyServer(function(input, output) {
  n = reactive({
    switch(input$n,
           "500" = 500,
           "1000" = 1000,
           "2000" = 2000)
  })
  x = reactive({ boot(n())})
  
  CI = reactive({
    p = input$p
    quantile(x(), c((1-p/100)/2, (1+p/100)/2))
  })
 
  output$plot = renderPlot({
    plot(density(x()), main = "Histrogram of Median Number of Annual Mile Driven in CA from Bootstrap")
    abline(v = CI()[1], col = "red")
    abline(v = CI()[2], col = "red")
    text(CI()[1] , 0.0005, "LowerBound")
    text(CI()[2] , 0.0005, "UpperBound")
    abline(v = mean(x()), col = "blue")
    text(mean(x()), 0.0009, paste("Estimated Median", mean(x()), sep = ""))
  }) 
  
  output$text1 = renderText({    
    mean(x())
  })
  
  output$text2 = renderText({ 
    var(x())
  })
  
  output$CItable = renderTable({
    matrix(CI(), nrow = 1, 
           dimnames = list("CI", c("LowerBound", "UpperBound")))
  })
  
  output$sTable = renderPrint({
    samTable
    
  })
  
  output$sPlot = renderPlot({
    samPlot
  })

})