# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(shinyRGL)
library(rgl)
load("data/satDF.rda")

shinyServer(function(input, output) {
  # Expression that generates a rgl scene with a number of points corresponding
  # to the value currently set in the slider.
  output$sctPlot <- renderWebGL({
    
    open3d()
    
    plot3d(satDF[, input$x3d], satDF[, input$y3d], satDF[, input$z3d], 
           col = "blue", size = 0.99, type = "s", xlab = input$x3d, 
           ylab = input$y3d, zlab = input$z3d)
    bbox3d(yat = c(900, 1000, 1100))
    aspect3d(1,1,1)
  })
})