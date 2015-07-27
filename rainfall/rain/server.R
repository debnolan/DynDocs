library(shiny)
library(ggmap)
library(extRemes)
source("helpers.R")
options(digits = 5)
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
    input$goButton
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
  #the map in Analysis tab
  output$mapImage <- renderImage({
    # When datasetInput() is 3, filename is www/images/3.png
    filename <- normalizePath(file.path('www/images',
                                        paste(datasetInput(), '.png', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  #the map in Simulation tab
  output$mapImage2 <- renderImage({
    filename <- normalizePath(file.path('www/images',
                                        paste(datasetInput2(), '.png', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  #The data plot
  output$plots3 <- renderPlot({
    
    thresh = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100)
    which(rainfall[[datasetInput()]]$precip>thresh)
    par(mfrow = c(1,1),bg = '#FAFAD2', mai = c(0.5,0.5,0.3,0.1))
    plot(rainfall[[datasetInput()]]$precip[which(rainfall[[datasetInput()]]$precip>thresh)]~rainfall[[datasetInput()]]$time[which(rainfall[[datasetInput()]]$precip>thresh)],
         ylim = range(rainfall[[datasetInput()]]$precip),
         xlab = "", ylab = "Precipitation (mm)", main = paste("Daily precipitation of weather station", datasetInput()), col = 'darkblue')
    points(rainfall[[datasetInput()]]$precip[which(rainfall[[datasetInput()]]$precip<=thresh)]~rainfall[[datasetInput()]]$time[which(rainfall[[datasetInput()]]$precip<=thresh)],
           col = 'black')
    abline(h = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100), col = 'red', lwd = 2)
    
  })
  
  #The 4 diagnostic plots
  output$plots2 <- renderPlot({
    fit = fevd(precip, rainfall[[datasetInput()]], threshold = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100), type="GP", units = "mm")
    par(mfrow = c(2,2), bg = '#FAFAD2',mai = c(0.45,0.45,0.3,0.1))
    plot(fit, "probprob", main ="Probability Plot")
    plot(fit, "qq", main ="Quantile Plot")
    plot(fit, type = "rl", rperiods= c(2,5,10,20,50,80,100,120,200), main ="Return Level Plot")
    plot(fit, type = "density", main = "Density Plot")
  })
  
  #The return levels
  output$rl <- renderText({
    RL2 = ci(fevd(precip, rainfall[[datasetInput()]],
                  threshold = quantile(rainfall[[datasetInput()]]$precip, input$threshold/100),
                  type="GP", units = "mm"),
             return.period=c(2,5,10,20,50,80,100,120,200))
    paste(capture.output(myprint(RL2)), collapse = "\n")
  })
  
  # Simulation QQ plot
  output$plots4 <-  renderPlot({
    par(mfrow = c(1,1),bg = '#FAFAD2', mai = c(0.5,0.5,0.3,0.1))
    plot(fevd(precip, rainfall[[datasetInput2()]],
              threshold = quantile(rainfall[[datasetInput2()]]$precip, input$threshold2/100),
              type="GP", units = "mm"),
         "qq2")
    title(paste("Simulation result (Weather station ", datasetInput2(), ")", sep = ""))
  })
  
  # If you don't like it, you can delete it
  # Mean Residual plot
  output$plots5 <-  renderPlot({
    par(mfrow = c(1,1),bg = '#FAFAD2', mai = c(0.8,0.8,0.3,0.1))
    #rainfalldf = as.data.frame(rainfall[[datasetInput()]])
    #mrlplot(rainfalldf$precip, nint =  input$threshold)
    mrlplot(rainfall[[datasetInput()]]$precip, nint =  input$threshold)
    title("Mean Residual Plot")
  })
  
})