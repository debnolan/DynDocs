library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.3")
load("../rda_data/satDF_new.rda")
library(ggplot2)
library(grid)

# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  
  output$text <- renderText({
    input$title
  })
  
  output$scatterPlot_1 <- renderPlot({
   x <- satDF_new[ , input$x_1]
   y <- satDF_new[ , input$y_1]
   
   dat <- data.frame(x, y, Region = satDF_new$Region, Population = satDF_new$`State Population`, 
                     satDF_new$State)

   p <- ggplot(dat, aes(x, y, size = Population, colour = Region), guide = FALSE)
   p + geom_point() + 
      xlab(input$x_1) +
      ylab(input$y_1) + 
      ggtitle(paste(input$y_1, "vs.", input$x_1)) +
      theme_bw() +
      theme(axis.title.x=element_text(vjust = -1),
               axis.title.y=element_text(angle = 90, vjust = 1),
               plot.title = element_text(colour = "black"),
#                plot.title=element_text(size = 15, vjust = 3),
               legend.position="top",
               legend.key = element_rect(colour = "white"))
  })
   
   output$scatterPlot_2 <- renderPlot({
     x <- satDF_new[ , input$x_2]
     y <- satDF_new[ , input$y_2]
     
     dat <- data.frame(x, y, Region = satDF_new$Region, Population = satDF_new$`State Population`, 
                       satDF_new$State)
     
     p <- ggplot(dat, aes(x, y, size = Population, colour = Region), guide = FALSE)
     p + geom_point() + 
       xlab(input$x_2) +
       ylab(input$y_2) + 
       ggtitle(paste(input$y_2, "vs.", input$x_2)) +
       theme_bw() +
       theme(axis.title.x=element_text(vjust = -1),
             axis.title.y=element_text(angle = 90, vjust = 1),
#              plot.title = element_text(colour = "black"),
             plot.title=element_text(size = 15, vjust = 3),
             legend.position="top", 
             legend.key = element_rect(colour = "white"))
  })
})