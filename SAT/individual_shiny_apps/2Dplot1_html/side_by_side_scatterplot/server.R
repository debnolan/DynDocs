library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(ggvis)


# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  load("data/satDF.rda")
  load("data/satDF_one.rda")
  
  output$text <- renderText({
    input$title
  })
  ###################### sidebyside_1
  output$sidebyside_1 <- renderPlot({
   x <- satDF[ , input$x_1]
   y <- satDF[ , input$y_1]
   
   dat <- data.frame(x, y, 
                     region = state.region, 
                     population = satDF_one$population, 
                     state_abb = state.abb)
   p <- ggplot(dat, aes(x, y, size = population, 
               colour = region, label = state_abb))
   
   if(identical(x, satDF$expend)| identical(x, satDF$salary)){
     p + geom_point(alpha = 0.8) + 
       ggtitle(paste(input$y_1, "vs.", input$x_1)) +
       xlab(paste(input$x_1, "(in '000s of dollars)")) +
       ylab(input$y_1) + 
       scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
       theme_bw() +
       theme(plot.title = element_text(size = 25),
             legend.position = "top", 
             legend.background = element_rect(fill = "transparent", colour = "transparent"),
             legend.key = element_rect(colour = "transparent"))
     }else{
       p + geom_point(alpha = 0.8) + 
         ggtitle(paste(input$y_1, "vs.", input$x_1)) +
         xlab(input$x_1) +
         ylab(input$y_1) + 
         scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
         theme_bw() +
         theme(plot.title = element_text(size = 25),
               legend.position = "top", 
               legend.background = element_rect(fill = "transparent", colour = "transparent"),
               legend.key = element_rect(colour = "transparent"))
     }}, height = 600, width = 600)
  
  ###################### sidebyside_2
   output$sidebyside_2 <- renderPlot({
     x <- satDF[ , input$x_2]
     y <- satDF[ , input$y_2]
     
     dat <- data.frame(x, y, 
                       region = state.region, 
                       population = satDF_one$population, 
                       state_abb = state.abb)
     p <- ggplot(dat, aes(x, y, size = population, 
                          colour = region, label = state_abb))
     
     if(identical(x, satDF$expend)| identical(x, satDF$salary)){
       p + geom_point(alpha = 0.8) + 
         ggtitle(paste(input$y_2, "vs.", input$x_2)) +
         xlab(paste(input$x_2, "(in '000s of dollars)")) +
         ylab(input$y_2) + 
         scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
         theme_bw() +
         theme(plot.title = element_text(size = 25),
               legend.position = "top", 
               legend.background = element_rect(fill = "transparent", colour = "transparent"),
               legend.key = element_rect(colour = "transparent"))
     }else{
       p + geom_point(alpha = 0.8) + 
         ggtitle(paste(input$y_2, "vs.", input$x_2)) +
         xlab(input$x_2) +
         ylab(input$y_2) + 
         scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
         theme_bw() +
         theme(plot.title = element_text(size = 25),
               legend.position = "top", 
               legend.background = element_rect(fill = "transparent", colour = "transparent"),
               legend.key = element_rect(colour = "transparent"))
     }}, height = 600, width = 600)
   
   ###################### hover info
   output$info_1 <- renderPrint({
     nearPoints(satDF, input$plot_hover_1, xvar = input$x_1, yvar = input$y_1, threshold = 7)
   })
   
   output$info_2 <- renderPrint({
     nearPoints(satDF, input$plot_hover_2, xvar = input$x_2, yvar = input$y_2, threshold = 7)
   })
     
})