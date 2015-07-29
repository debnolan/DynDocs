# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(shinyRGL)
library(rgl)
#' Define server logic required to generate and 3d scatterplot

shinyServer(function(input, output) {
  load("data/satDF.rda")
  # Expression that generates a rgl scene with a number of points corresponding
  # to the value currently set in the slider.
  output$sctPlot <- renderWebGL({
   
    x<- satDF[ , input$x] 
    y<- satDF[ , input$y] 
    z<- satDF[ , input$z]

    plot3d(x, y, z, col = "blue", size = 0.99, type = "s", xlab = input$x, 
           ylab = input$y, zlab = input$z)
    aspect3d(1,1,1)
  })
})
