library(shiny)
options(rgl.useNULL=TRUE)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(ggvis)
library(shinyRGL)
library(rgl)
library(plyr)


# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  load("data/satDF.rda")
  load("data/satDF_one.rda") # adding two columns: region and state population
  
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
   
   ###################### data display
   load("data/satDF_pretty.rda")
   
   output$mytable1 = renderTable({
     satDF_pretty
   }, options = list(orderClasses = TRUE))
   
   ###################### 3D plot
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
   
   ###################### Simpson's Paradox tutorial
   count_rate_values <- reactive({
     success_rate_A_small = 0.93
     success_rate_A_large = 0.73
     success_rate_B_small = 0.87
     success_rate_B_large = 0.69
     
     # Treatment A values
     success_count_A_small = 
       round(success_rate_A_small * input$count_A_small, digits = 0)
     success_count_A_large = 
       round(success_rate_A_large * (350 - input$count_A_small), digits = 0)
     success_count_A_both = 
       round((success_count_A_small + success_count_A_large), digits = 0)
     success_rate_A_both =
       round((success_count_A_small + success_count_A_large)/ 350 * 100, digits = 0)
     
     # Treatment B values
     success_count_B_small = 
       round(success_rate_B_small * input$count_B_small, digits = 0)
     success_count_B_large = 
       round(success_rate_B_large * (350 - input$count_B_small), digits = 0)
     success_count_B_both = 
       round((success_count_B_small + success_count_B_large), digits = 0)
     success_rate_B_both = 
       round((success_count_B_small + success_count_B_large)/ 350 *100, digits = 0)
     
     df<- data.frame(countA = c(success_count_A_small, success_count_A_large, success_count_A_both), 
                     rateA = c(success_rate_A_small, success_rate_A_large, success_rate_A_both),
                     countB = c(success_count_B_small, success_count_B_large, success_count_B_both),
                     rateB = c(success_rate_B_small, success_rate_B_large, success_rate_B_both)
     )
   })
   
   cell_values <- reactive({
     df <- count_rate_values()
     df <- apply(df, 2, as.character)
     cell11count = paste(df[1,1], "/", as.character(input$count_A_small), sep = "")
     cell21count = paste(df[2,1], "/", as.character(350 - input$count_A_small), sep = "")
     cell31count = paste(df[3,1], "/", "350", sep = "")
     cell11rate = paste(as.numeric(df[1,2]) * 100, "%", sep = "")
     cell21rate = paste(as.numeric(df[2,2]) * 100, "%", sep = "")
     cell31rate = paste(df[3,2], "%", sep = "")
     
     cell12count = paste(df[1,3], "/", as.character(input$count_B_small), sep = "")
     cell22count = paste(df[2,3], "/", as.character(350 - input$count_B_small), sep = "")
     cell32count = paste(df[3,3], "/", "350", sep = "")
     cell12rate = paste(as.numeric(df[1,4]) * 100, "%", sep = "")
     cell22rate = paste(as.numeric(df[2,4]) * 100, "%", sep = "")
     cell32rate = paste(df[3,4], "%", sep = "")
     
     df <- data.frame(
       cases = c("Small Stones", "Large Stones", "Both"),
       treatmentA = c(paste(cell11count, " ", "(", cell11rate, ")", sep = ""), 
                      paste(cell21count, " ", "(", cell21rate, ")", sep = ""), 
                      paste(cell31count, " ", "(", cell31rate, ")", sep = "")),
       treatmentB = c(paste(cell12count, " ", "(", cell12rate, ")", sep = ""), 
                      paste(cell22count, " ", "(", cell22rate, ")", sep = ""), 
                      paste(cell32count, " ", "(", cell32rate, ")", sep = "")),
       
       stringsAsFactors = FALSE)
     
     rename(df, c("cases"= "Severity",
                  "treatmentA" = "Treatment A", 
                  "treatmentB" = "Treatment B"))
   }) 
   
   
   # Show the values using an HTML table
   output$table <- renderTable({cell_values()})
   
   output$yesorno <- renderText({
     df<- count_rate_values()
     if(df[3,2] < df[3,4]){
       print("Simpson's Paradox: YES")
     }else{
       print("Simpson's Paradox: NO")
     }
   })  
})