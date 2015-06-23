library(shiny)
library(survey)
library(tables)
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
    plot(density(x))
  }) 
  
  output$text1 = renderText({ 
    mean(boot(n()))
  })
  
  output$text2 = renderText({ 
    var(boot(n()))
  })
  
  output$table = renderTable({
    print(table)
  })
  
  
})