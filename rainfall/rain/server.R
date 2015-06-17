library(shiny)
library(ggmap)
library(extRemes)
source("helpers.R")
shinyServer(function(input, output) {
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$station,
           "Weather station 1" =1, "Weather station 2" =2, "Weather station 3" =3, "Weather station 4" =4,
           "Weather station 5" =5, "Weather station 6" =6, "Weather station 7" =7, "Weather station 8" =8, 
           "Weather station 9" =9, "Weather station 10" =10, "Weather station 11" =11, "Weather station 12" =12,
           "Weather station 13" =13, "Weather station 14" =14, "Weather station 15" =15, "Weather station 16" =16,
           "Weather station 17" =17, "Weather station 18" =18, "Weather station 19" =19, "Weather station 20" =20,
           "Weather station 21" =21, "Weather station 22" =22, "Weather station 23" =23, "Weather station 24" =24,
           "Weather station 25" =25, "Weather station 26" =26, "Weather station 27" =27, "Weather station 28" =28,
           "Weather station 29" =29, "Weather station 30" =30, "Weather station 31" =31, "Weather station 32" =29,
           "Weather station 33" =33, "Weather station 34" =34, "Weather station 35" =35, "Weather station 36" =36,
           "Weather station 37" =37, "Weather station 38" =38, "Weather station 39" =39, "Weather station 40" =40,
           "Weather station 41" =41, "Weather station 42" =42, "Weather station 43" =43, "Weather station 44"= 44,
           "Weather station 45" =45, "Weather station 46" =46, "Weather station 47" =47, "Weather station 48"= 48,
           "Weather station 49" =49, "Weather station 50" =50, "Weather station 51" =51, "Weather station 52" =52,
           "Weather station 53"= 53, "Weather station 54" =54, "Weather station 55" =55, "Weather station 56" =56)
  })
  # Generate a summary of the dataset
  output$plots1 <- renderPlot({
    plot_map(datasetInput())
  })
  output$plots2 <- renderPlot({
    fit = fevd(precip, rainfall[[datasetInput()]], threshold = input$threshold, type="GP", units = "mm")
    par(mfrow = c(2,2))
    #  plot(rainfall[[5]]$precip ~ rainfall[[5]]$time, xlab = "", ylab = "Precipitation (mm)", main = paste("Daily precipitation of weather station", 5))
    plot(fit, "probprob", main ="Probability Plot")
    plot(fit, "qq", main ="Quantile Plot")
    plot(fit, type = "rl", rperiods= c(2,5,10,20,50,80,100,120,200), main ="Return Level Plot")
    plot(fit, type = "density", main = "Density Plot")
  })
})