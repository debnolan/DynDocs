library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)

# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  load("../rda_data/satDF_one.rda")
  
  output$text <- renderText({
    input$title
  })
  
  dataset <- reactive({
  data.frame(state.abb, satDF_one[, c(input$x_1, input$y_1, "region", "population")])
  })
  
  environment<-environment() 
  output$scatterPlot_1 <- renderPlot({
  selectedData <- dataset()
  p <- ggplot(selectedData, aes_string(selectedData[input$x_1], selectedData[input$y_1], 
                                    size = "population", colour = "region",
                                    label = "state.abb"))
  
   if(identical(selectedData[input$x_1], "Expenditure Per Pupil") | 
                identical(selectedData[input$x_1], satDF_one$salary)){
     p + geom_point(alpha = 0.5) + 
       geom_text(size = 3, colour = "black", vjust = -1) +
       xlab(paste(input$x_1, "(in '000s of dollars)")) +
       ylab(input$y_1) + 
       ggtitle(paste(input$y_1, "vs.", input$x_1)) +
       scale_size(name = "Population in\n1994") +
       # hide population legend
       # scale_size_continuous(guide = FALSE) +
       theme_bw() +
       theme(axis.title.x = element_text(vjust = 0.4),
             axis.title.y = element_text(angle = 90, vjust = 1),
             plot.title = element_text(size = 15, vjust = 3),
             legend.position = c(0.88, 0.73), 
             legend.background = element_rect(fill = "transparent", colour = "transparent"),
             legend.key = element_rect(colour = "transparent"))  
   }
#   else{
#      p + geom_point(alpha = 0.5) + 
#        geom_text(size = 3, colour = "black", vjust = -1) +
#        xlab(input$x_1) +
#        ylab(input$y_1) + 
#        ggtitle(paste(input$y_1, "vs.", input$x_1)) +
#        scale_size(name = "Population in\n1994") +
#        # hide population legend
#        # scale_size_continuous(guide = FALSE) +
#        theme_bw() +
#        theme(axis.title.x = element_text(vjust = 0.4),
#              axis.title.y = element_text(angle = 90, vjust = 1),
#              plot.title = element_text(size = 15, vjust = 3),
#              legend.position = c(0.88, 0.73), 
#              legend.background = element_rect(fill = "transparent", colour = "transparent"),
#              legend.key = element_rect(colour = "transparent"))  
#    }
#    
  })
})
#    output$scatterPlot_2 <- renderPlot({
#      x <- satDF_new[ , input$x_2]
#      y <- satDF_new[ , input$y_2]
#      
#      dat <- data.frame(x, y, Region = satDF_new$Region, Population = satDF_new$`State Population`, 
#                        satDF_new$State)
#      
#      p <- ggplot(dat, aes(x, y, size = sqrt(Population/pi), colour = Region), guide = FALSE)
#      p + geom_point() + 
#        xlab(input$x_2) +
#        ylab(input$y_2) + 
#        ggtitle(paste(input$y_2, "vs.", input$x_2)) +
#        scale_size(range = c(0, 15)) +
#        theme_bw() +
#        theme(axis.title.x=element_text(vjust = -1),
#              axis.title.y=element_text(angle = 90, vjust = 1),
# #              plot.title = element_text(colour = "black"),
#              plot.title=element_text(size = 15, vjust = 3),
#              legend.position="top", 
#              legend.key = element_rect(colour = "white"))
#   })
# })