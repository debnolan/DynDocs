require(shiny)

source("analysis.r")

load("data/mnist.RData")


shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      
      
      
    })
  }
)


