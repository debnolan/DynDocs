# Define server logic for random distribution application
shinyServer(function(input, output) {
  
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
  # plot label.
  
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
