library(shiny)
library(ggmap)
library(extRemes)
source("helpers.R")
options(digits = 9)
shinyServer(function(input, output,session) {
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
  datasetInput2 = reactive({
    switch(input$station2,
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
  
  
  output$mapImage <- renderImage({
    # When datasetInput() is 3, filename is www/images/3.png
    filename <- normalizePath(file.path('www/images',
                                        paste(datasetInput(), '.png', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$mapImage2 <- renderImage({
    filename <- normalizePath(file.path('www/images',
                                        paste(datasetInput2(), '.png', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  output$plots3 <- renderPlot({
    thresh = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100)
    which(rainfall[[datasetInput()]]$precip>thresh)
    par(bg = '#FAFAD2')
    plot(rainfall[[datasetInput()]]$precip[which(rainfall[[datasetInput()]]$precip>thresh)]~rainfall[[datasetInput()]]$time[which(rainfall[[datasetInput()]]$precip>thresh)],
         ylim = range(rainfall[[datasetInput()]]$precip),
         xlab = "", ylab = "Precipitation (mm)", main = paste("Daily precipitation of weather station", datasetInput()), col = 'darkblue')
    points(rainfall[[datasetInput()]]$precip[which(rainfall[[datasetInput()]]$precip<=thresh)]~rainfall[[datasetInput()]]$time[which(rainfall[[datasetInput()]]$precip<=thresh)],
           col = 'black')
    abline(h = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100), col = 'red', lwd = 2)
  })
  
  
  
  output$plots2 <- renderPlot({
    fit = fevd(precip, rainfall[[datasetInput()]], threshold = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100), type="GP", units = "mm")
    par(mfrow = c(2,2), bg = '#FAFAD2')
    plot(fit, "probprob", main ="Probability Plot")
    plot(fit, "qq", main ="Quantile Plot")
    plot(fit, type = "rl", rperiods= c(2,5,10,20,50,80,100,120,200), main ="Return Level Plot")
    plot(fit, type = "density", main = "Density Plot")
  })
  
#   simParams <- reactiveValues(station = NULL, thresh = NULL)
#   
#   observeEvent( input$goButton, {
#     simParams$station = datasetInput2()
#     simParams$thresh = input$threshold2
#   })
#   
  
  output$rl2 <- renderText({
    RL2 = ci(fevd(precip, rainfall[[data()$data]],
                  threshold = quantile(rainfall[[data()$data]]$precip, data()$thresh/100),
                  type="GP", units = "mm"),
             return.period=c(2,5,10,20,50,80,100,120,200))
    paste(capture.output(myprint(RL2))[-(1:3)], collapse = "\n")
  })
  
  output$plots4 <- renderPlot({
    par(bg = '#FAFAD2')
    plot(fevd(precip, rainfall[[data()$data]],
              threshold = quantile(rainfall[[data()$data]]$precip, data()$thresh/100),
              type="GP", units = "mm"),
         "qq2")
    title(paste("Simulation result (Weather station ", data()$data, ")", sep = ""))
  })
  
  
  data <- reactive({
    input$goButton
    isolate({
      st <-  switch(input$station2,
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
      list(data = st, thresh = input$threshold2)
    })
  })
  
  
  
  
  
  
  
  
})