library(HMM)
library(expm)
library(shiny)



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output expressions defined 
  # below then all used the value computed from this expression
  data0 <- reactive({
    rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
    (rsmat0 %^% 100)[1, ]
  })
  
  data1 <- reactive({
    statdist = function( ini, k){ 
      rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
      sapply(0:k, function(i) ini %*% (rsmat0 %^% i) )
    }
    statdist(ini = c(input$initr, 1 - input$initr), k = input$n)[1, ]
  })
  
  data2 <- reactive({
    statdist = function( ini, k){ 
      rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
      sapply(0:k, function(i) ini %*% (rsmat0 %^% i) )
    }
    statdist(ini = c(input$initr, 1 - input$initr), k = input$n)[2, ]
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    plot(data1() , type = "l", ylim = c(0,1), col = "blue", xlab = "Steps", 
         ylab = "Probability of Sun and Rain")
    lines(data2() , col = "red")
    abline(h = data0()[1], lwd = 2, col = "blue")
    abline(h = data0()[2], lwd = 2, col = "red")
    legend(x = input$n - (input$n / 15) + 1, y = 0.2, legend = c("sun", "rain"), fill = , c("red", "blue"))
    text(x = 1, y = data1()[1], labels = as.character(input$initr))
    text(x = 1, y = data2()[1], labels = as.character(1 - input$initr))
  })
  
  # Generate a summary of the data
#  output$summary <- renderPrint({
#    summary(data())
#  })
  
  # Generate an HTML table view of the data
#  output$table <- renderTable({
#    data.frame(x=data())
#  })
  
})
