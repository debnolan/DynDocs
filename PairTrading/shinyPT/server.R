library(shiny)
source("helper.R")

funx = function( data ) {
  r = data[,2]/stock[,3]
  plotRatio(r)
}


shinyServer( function( input,output) {
  
  data <- reactive({ 
    combine2Stocks( input$a, input$b )
  })
  
  output$plot <- renderPlot({
    funx (stock)
  })

})