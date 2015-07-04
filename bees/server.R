# server.R
source("helper.R")

shinyServer(function(input, output) {
  
  data <- reactive({
    
    input$runSim
    
    isolate({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     gamma = rgamma,
                     rnorm)
      list(data = dist(input$n), name = input$dist, n = input$n)
    })
  })
    ##add if case to account for alpha and beta parameters - above and below

    output$plot1 <- renderPlot({
      
      if (input$cdf) {
        plot(ecdf(data()$data),
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
      }
      else {
        hist(data()$data, 
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
      }
    })
    
    plotParams <- reactiveValues(alpha = 1, beta = 1)
    
    observeEvent( input$makePlot, {
      plotParams$alpha = input$alpha
      plotParams$beta = input$beta
    })
    
    output$plot2 <- renderPlot({  
      graphGamma(alpha = plotParams$alpha, beta = plotParams$beta)
    })
})
