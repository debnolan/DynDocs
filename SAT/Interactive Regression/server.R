library(shiny)
library(RColorBrewer)

shinyServer(function(input, output) {
  load("data/satDF_new.rda")

 regression <- reactive({
    
    # Get the current model structure
    data <- data.frame(  
      SAT = satDF_new$'SAT Total Score',
      Frac = satDF_new$'Eligible Student Fraction',
                       Salary = satDF_new$'Teacher Salary')
  
    # Conditionally fit the model
    if (input$model == "Simple regression") {
      fit.res <- lm( data$SAT~ data$Salary, data)
    } else if (input$model == "Controlling for frac") {
      fit.res <- lm(data$SAT~ data$Salary + data$Frac, data)
    } 
    
    # Get the model summary
    if (is.null(fit.res)) {
     fit.summary <- NULL
   }  else {
      fit.summary <- summary(fit.res)
    }
    
    return(list(fit.res=fit.res, fit.summary=fit.summary))
    
  })
  
  #---------------------------------------------------------------------------
  # Plot a scatter of the data with regression lines corresponding to the model
  output$reg.plot <- renderPlot({         
    # Get the current regression data
  
    x <- data$Salary
    y <- data$SAT
    g <- data$Frac
    coefs <- regression()$fit.res$coefficients
    
   # print(coefs)
    
    
    # Plot the true model
    
    plot(x, y, pch=16, cex=1.2, col="#333333", bty="n", xlab="x", ylab="y")
   
    if (input$model == "Simple regression") {
      abline(coefs[1], coefs[2], col="#333333", lwd=3)
    } else if (input$model == "Controlling for frac") {
      abline(coefs[1], coefs[2], col="#333333", lwd=3)
      abline(coefs[1] + coefs[3], coefs[2], lwd=3)
    } 
    
  })
  
  #---------------------------------------------------------------------------
  # Show the lm() summary for the 
  output$reg.summary <- renderPrint({
    
    summary <- regression()$fit.summary
    if (!is.null(summary)) {
      return(regression()$fit.summary)
    }
    
  })
  
})