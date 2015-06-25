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
 
  output$plot = renderPlot({
    plot(density(x()), main = "Histrogram of Median Number of Annual Mile Driven in CA from Bootstrap")
  }) 
  
  output$text1 = renderText({    
    mean(x())
  })
  
output$text2 = renderText({ 
    var(x())
  })
  
  output$CItable = renderTable({
    df = data.frame(LowerBound = t(quantile(x(), c(0.025, 0.975)))[1], 
               UpperBound = t(quantile(x(), c(0.025, 0.975)))[2])
    rownames(df) = c("CI")
    print(df)
  })


})