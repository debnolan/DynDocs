library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.1")
load("satDF_new.rda")
# Note! Need to rename the variables
library(ggplot2)
library(grid)

# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
#   formulaText <- reactive(function() {
#     paste(input$y_1, "vs.", input$x_1)
#   })
#   
#   # Return the formula text for printing as a caption
#   output$caption <- reactiveText(function() {
#     formulaText()
#   })
  
  output$text <- renderText({
    input$title
  })
  
  output$scatterPlot_1 <- renderPlot({
   x <- satDF_new[ , input$x_1]
   y <- satDF_new[ , input$y_1]
   
   # par(mar = c(5.1, 4.1, 0, 1))
   # plot(x, y, pch = 19, col = "blue", cex = 2) 
  
   dat <- data.frame(x, y, Region = satDF_new$region, Population = satDF_new$StatePopulation, 
                     satDF_new$State)

   p <- ggplot(dat, aes(x, y, size = Population), guide = FALSE)
   p + geom_point(aes(colour = Region)) + 
      xlab(input$x_1) +
      ylab(input$y_1) + 
      ggtitle(paste(input$y_1, "vs.", input$x_1)) +
      theme_bw() +
      theme(axis.title.x=element_text(vjust = -1),
               axis.title.y=element_text(angle = 90, vjust = 1),
               plot.title = element_text(colour = "black"),
               plot.title=element_text(size = 15, vjust = 3),
               legend.position="top",
               legend.key = element_rect(colour = "white"))
#       theme(plot.margin = unit(c(0,0,0,0), "cm")) 
#       theme(legend.background = element_rect(), legend.margin = unit(0, "cm"))
  })
   
   output$scatterPlot_2 <- renderPlot({
     x <- satDF_new[ , input$x_2]
     y <- satDF_new[ , input$y_2]
     
     # par(mar = c(5.1, 4.1, 0, 1))
     # plot(x, y, pch = 19, col = "blue", cex = 2) 
     
     dat <- data.frame(x, y, Region = satDF_new$region, Population = satDF_new$StatePopulation, 
                       satDF_new$State)
     
     p <- ggplot(dat, aes(x, y, size = Population), guide = FALSE)
     p + geom_point(aes(colour = Region)) + 
       xlab(input$x_2) +
       ylab(input$y_2) + 
       ggtitle(paste(input$y_2, "vs.", input$x_2)) +
       theme_bw() +
       theme(axis.title.x=element_text(vjust = -1),
             axis.title.y=element_text(angle = 90, vjust = 1),
             plot.title = element_text(colour = "black"),
             plot.title=element_text(size = 15, vjust = 3),
             legend.position="top", 
             legend.key = element_rect(colour = "white"))
  })
})