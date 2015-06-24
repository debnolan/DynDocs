library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output expressions defined 
  # below then all used the value computed from this expression
  data <- reactive({

    input$runSim

    isolate({
      dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
      list(data = dist(input$n), name = input$dist, n = input$n)
     })
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    
    if (input$cdf) {
      plot(ecdf(data()$data),
           main=paste('r', data()$name, '(', data()$n, ')', sep=''))
    }
    else {
       hist(data()$data, 
            main=paste('r', data()$name, '(', data()$n, ')', sep=''))
    }
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data()$data)
  })
  
})
