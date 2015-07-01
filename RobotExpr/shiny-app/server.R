# server.R

library(shiny)
library(quantmod)
source("helpers.R")

shinyServer(
  function(input, output) {
    
    data <- reactiveValues(funcType = NULL, a = NULL, b = NULL, sigma = NULL, n = NULL)
    
    observeEvent( input$get, {      
      data$funcType = input$func
      data$a = input$a
      data$b = input$b
      data$n = input$range
      data$sigma = input$sig
    })
      

    output$plot <- renderPlot({
      
      if (is.null(data$funcType)) return(NULL)
      
      color = switch(data$funcType,
                          "y1" = "darkgreen",
                          "y2" = "darkviolet")
      
      title = switch(data$funcType,
                                  "y1" = "Non-linear Exponential",
                                  "y2" = "Linear Exponential")
      
      vals = exp_curve(func = data$funcType, sig = data$sigma, a = data$a, 
                       b = data$b, n = data$n)
      
      logScale = input$log
      
      if (logScale){
        plot(vals$y~vals$x, col = color, pch = 20, main = title, xlab = "", 
           ylab = "", log = "y") 
      }else{
        plot(vals$y~vals$x, col = color, pch = 20, main = title, xlab = "", 
             ylab = "") 
      }
  })
  }
)

