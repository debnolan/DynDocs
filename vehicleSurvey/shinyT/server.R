
library(shiny)
y = c(-4, 3, 7, 5) # this is the bootstrap population
bootSamp = expand.grid(X1 = y, X2 = y , X3 = y, X4 = y) # totally 256 outcomes of resamples
bootSamp$mean = apply(as.matrix(bootSamp), 1, mean) # add a column of the mean of each sample 
bootMean = bootSamp$mean 


shinyServer(function(input, output) {
   ### output for exact bootstrap 
    output$summary <- renderTable({
      data.frame(table(bootMean))
    }) # It is a table of summarizes the mean of each sample, non interactive.
    
    output$table <- renderTable({ 
      bootSamp
    }) # It prints out the dataframe containing all possible samples from the bootstrap population.
    
    
    output$plot <- renderPlot({ 
      x = bootMean
      bins = seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, main = "Exact Bootstrap Sampling Distribution", 
           xlab = "mean", col = 'turquoise', border = 'white')
      abline(v = mean(x), col = "black")
      text(mean(x), 25,  paste('Estimated Mean = ', mean(x), sep = ""))
    }) # Shows the distribution of the means of all bootstrap samples.
    
    
    ### data & output for bootstrap estimation
    
    data = reactive({
      input$runSim #submit button, graph only change upon clicking this button
      
      isolate({
      dist = switch(input$dist,
                     unif = runif(10000),
                     exp = rexp(10000),
                     gamma = rgamma(10000, 0.75),
                     runif) ## 3 choices of population 
      
      pop = dist
      samp = sample(pop, input$n) ## create an sample with the chosen population and sample size
      boot = replicate(input$B, sample(samp, input$n, replace = T))# use the chosen sample as bootstrap population
      srs = replicate(input$B, sample(pop, input$n, replace = T))# SRS with replacement from population
     
      statistic = switch(input$stat,
                    "mean" = mean,
                    "median" = median,
                    mean) 
      stat = statistic
      boot_stat = apply(boot, 2, stat) #compute the statistic from bootstrap samples
      true_stat = apply(srs, 2, stat) #compute the statistic from random sampling samples
      
      list(pop = pop, samp = samp, boot = boot, 
           name = input$dist, n = input$n, B = input$B, s = input$stat,
           boot_stat = boot_stat, true_stat = true_stat,
           stat = stat)
      # I have to put all inputs in the list, so some parts of the graph, such as the title
      #until I hit the summit botton)
    
      })# this list are the elements in the data variable
    })
     
    
    output$plot1 <- renderPlot({
      
      if (input$population) { ## create a summit button to change the view to this plot 
        
        hist(data()$true_stat, main = "Sampling Distribution", 
             xlab = paste('Value of the ', data()$s ))
        # histogram of the distribution of the chosen statistic from srs samples.
        abline(v = mean(data()$true_stat), col = "red")
        # show the mean of the statistics as the estimated statistic
        mtext(paste('estimated ', data()$s, '=', 
               sprintf("%.3f", mean(data()$true_stat)), sep = ''), 
              side = 3, line = 0 )
       
      }
      else { ## display the histogram of the chosen population
        hist(data()$pop, xlab = "Population Distribution",
             main=paste('r', data()$name, '(10000)', sep=''))
        abline(v = data()$stat(data()$pop), col = "red")
        ## the line shows either mean of median depends on the input of "data()$stat"
        mtext(paste(data()$s, '=', 
                    sprintf("%.3f", data()$stat(data()$pop)), sep = ''),
              side = 3, line = 0)
      }
    })
    
    
    
    output$plot3 <- renderPlot({
      if (input$true) {#create a summit button to change the view to this plot 
         hist(data()$boot_stat, main = "Bootstrap Sampling Distribution",
              xlab = paste('Value of the ', data()$s ))
         # histogram of the distribution of the chosen statistic from bootstrap samples.
        abline(v = mean(data()$boot_stat), col = "blue")
        # show the estimated statistic
        mtext(paste('estimated ', data()$s, '=', 
                    sprintf("%.3f", mean(data()$boot_stat)), sep = ''), 
              side = 3, line = 0 )
      }
      else {
        hist(data()$samp, xlab = "Sample Distribution",
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
        abline(v = data()$stat(data()$samp), col = "blue")
        mtext(paste(data()$s, '=', 
                    sprintf("%.3f", data()$stat(data()$samp)), sep = ''),
              side = 3, line = 0)
      }
      
    })
    
   
})