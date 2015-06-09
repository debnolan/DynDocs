library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.1")
load("satDF_new.rda")
# Note! Need to rename the variables
library(ggplot2)
<<<<<<< HEAD

# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  formulaText <- reactive(function() {
    paste(input$y, "vs.", input$x)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- reactiveText(function() {
    formulaText()
  })
  
  output$text <- renderText({
    input$title
  })  
=======
library(grid)

# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
#   formulaText <- reactive(function() {
#     paste(input$y, "vs.", input$x)
#   })
#   
#   # Return the formula text for printing as a caption
#   output$caption <- reactiveText(function() {
#     formulaText()
#   })
  
  output$text <- renderText({
    input$title
  })
  
>>>>>>> greentea
  output$scatterPlot <- renderPlot({
   x <- satDF_new[ , input$x]
   y <- satDF_new[ , input$y]
   
<<<<<<< HEAD
   # par(mar = c(5.1, 4.1, 0, 1))
=======
   par(mar = c(5.1, 4.1, 0, 1))
>>>>>>> greentea
   # plot(x, y, pch = 19, col = "blue", cex = 2) 
  
   dat <- data.frame(x, y, Region = satDF_new$region, Population = satDF_new$StatePopulation, 
                     satDF_new$State)

   p <- ggplot(dat, aes(x, y, size = Population), guide = FALSE)
   p + geom_point(aes(colour = Region)) + 
<<<<<<< HEAD
     xlab(input$x) +
     ylab(input$y)
=======
      xlab(input$x) +
      ylab(input$y) + 
      ggtitle(paste(input$y, "vs.", input$x)) +
      theme(axis.title.x=element_text(vjust = -1)) +
      theme(axis.title.y=element_text(angle = 90, vjust = 1)) +
      theme(plot.title = element_text(colour = "black")) +
      theme(plot.title=element_text(size = 15, vjust = 3)) 
     
#      + theme(plot.margin = unit(c(0,0,0,0), "cm"))
#      + theme(legend.background = element_rect(), legend.margin = unit(0, "cm"))
#      
>>>>>>> greentea
   

   
   
  
  })
})