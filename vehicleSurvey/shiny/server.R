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
 
  output$plot = renderPlot({
    x = boot(n())
    plot(density(x), main = "Histrogram of Median Number of Annual Mile Driven in CA from Bootstrap")
  }) 
  
  output$text1 = renderText({ 
    mean(boot(n()))
  })
  
  output$text2 = renderText({ 
    var(boot(n()))
  })
  
  output$CItable = renderTable({
    data.frame(LowerBound = t(quantile(boot(n()), c(0.025, 0.975)))[1], 
               UpperBound = t(quantile(boot(n()), c(0.025, 0.975)))[2])
  })

  
})