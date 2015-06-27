
library(shiny)
y = c(-4, 3, 7, 5)
bootSamp = expand.grid(X1 = y, X2 = y , X3 = y, X4 = y)
bootSamp$mean = apply(as.matrix(bootSamp), 1, mean)
bootMean = bootSamp$mean


shinyServer(function(input, output) {
   ### output for exact bootstrap 
    output$summary <- renderTable({
      t(data.frame(table(bootMean)))
    })
    
    output$table <- renderTable({ 
      bootSamp
     
    })
    
    output$plot <- renderPlot({ 
      x = bootMean
      bins = seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, main = "Exact Bootstrap Sampling Distribution", 
           xlab = "mean", col = 'turquoise', border = 'white')
      abline(v = mean(x), col = "black")
    })
    
    ### data & output for bootstrap estimation
    
    data = reactive({
      input$runSim
      
      isolate({
      dist = switch(input$dist,
                     unif = runif,
                     exp = rexp,
                     runif)  
      
      pop = dist(10000)
      samp = sample(pop, input$n)
      boot = replicate(input$B, sample(samp, input$n, replace = T))
      
      type = switch(input$stat,
                    "mean" = mean,
                    "median" = median,
                    mean)  
      stat = apply(boot, 2, type)
      list(pop = pop, samp = samp, boot = boot, 
           name = input$dist, n = input$n, stat = stat )
      })
    })
     
    
    output$plot1 <- renderPlot({
      
      if (input$population) {
        hist(data()$pop,
             main=paste('r', data()$name, '(10000)', sep=''))
      }
      else {
        hist(data()$samp,
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
      }
    })
    
    output$plot3 <- renderPlot({
      hist(data()$stat)
      abline(v = mean(data()$stat), col = "blue")
    })
    
   
})