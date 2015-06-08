library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.1")
load("satDF_new.rda")
# Note! Need to rename the variables
library(ggplot2)

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
  output$scatterPlot <- renderPlot({
   x <- satDF_new[ , input$x]
   y <- satDF_new[ , input$y]
   
   # par(mar = c(5.1, 4.1, 0, 1))
   # plot(x, y, pch = 19, col = "blue", cex = 2) 
  
   dat <- data.frame(x, y, Region = satDF_new$region, Population = satDF_new$StatePopulation, 
                     satDF_new$State)

   p <- ggplot(dat, aes(x, y, size = Population), guide = FALSE)
   p + geom_point(aes(colour = Region)) + 
     xlab(input$x) +
     ylab(input$y)
   

   
   
  
  })
})