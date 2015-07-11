library(shiny)
library(RColorBrewer)
library(ggplot2)

#getwd()
#setwd("~/DynDocs/SAT/Interactive Regression")

shinyServer(function(input, output) {
  load("data/satDF_new.rda")

  data <- data.frame(  
    SAT = satDF_new$'SAT Total Score',
    Frac = satDF_new$'Eligible Student Fraction',
    Salary = satDF_new$'Teacher Salary')
  
  frac_intervals = co.intervals(data$Frac, number = 5)
  
  # write a function that extract 6 frac groups
  frac_group = function(interval_group){
    which(data$Frac >= interval_group[1] & data$Frac <= interval_group[2])
  }
  
  frac_list = apply(frac_intervals, 1, frac_group)
  
  frac_list[[1]]
  
  lm(data$Frac[frac_list[[1]]] ~ data$Salary[frac_list[[1]]], data = data)
  
  data$Salary[frac_list[[1]]]
  
  lm_list = list()
  for(i in 1:5){
    lm_list[[i]] = lm(data$SAT[frac_list[[i]]] ~ data$Salary[frac_list[[i]]], data = data)
  }
   

regression <- reactive({

    if (input$model == "Simple regression") {
      fit.res <- lm(data$SAT~ data$Salary, data)
    } else if (input$model == "Controlling for frac") {
      fit.res <- lm(data$SAT~ data$Salary + data$Frac, data)
    } else if (input$model == "First Fifth of Data") {
      fit.res <- lm_list[[1]]
    } else if (input$model == "Second Fifth of Data") {
      fit.res <- lm_list[[2]]
    } else if (input$model == "Third Fifth of Data") {
      fit.res <- lm_list[[3]]
    } else if (input$model == "Fourth Fifth of Data") {
      fit.res <- lm_list[[4]]
    } else if (input$model == "Last Fifth of Data") {
      fit.res <- lm_list[[5]]
    } 

    #print("fit.res")
    #print(fit.res)
    
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
  
    x <- data$Salary
    y <- data$SAT
    g <- data$Frac
    coefs <- regression()$fit.res$coefficients
    
    #fraccoefs = lm_list()$coefficients
    
    #print("fraccoefs")
    #print(fraccoefs)
    #print(coefs)
    #print("coefs[1]")
    #print(coefs[1])
    
    # Plot the true model
    
  plot(x, y, pch=16, cex=1.2, bty="n", main= "The Effect of Teacher Salary on SAT scores", xlab="Average Teacher Salary (in thousands of dollars)", ylab="Average Total SAT Scores")
   
    if (input$model == "Simple regression") {
      #abline(coefs[1], coefs[2], lwd=3)
      abline(coefs, lwd=3)
    } else if (input$model == "Controlling for frac") {
      abline(a = coefs[1], b = coefs[2], lwd=3) 
      #abline(coefs["(Intercept)"], coefs[2], lwd=3) 
    } else if (input$model == "First Fifth of Data") {
      abline(lm_list[[1]]$coefficients, lwd=3) 
      par(col="red" )
    } else if (input$model == "Second Fifth of Data") {
      abline(lm_list[[2]]$coefficients, lwd=3)
    }  else if (input$model == "Third Fifth of Data") {
      abline(lm_list[[3]]$coefficients, lwd=3)
    }  else if (input$model == "Fourth Fifth of Data") {
      abline(lm_list[[4]]$coefficients, lwd=3)
    }  else if (input$model == "Last Fifth of Data") {
      abline(lm_list[[5]]$coefficients, lwd=3)
    }  
  })
  
  #---------------------------------------------------------------------------
  # Show the lm() summary for the 
 # output$reg.summary <- renderPrint({
    
  #  summary <- regression()$fit.summary
  #  if (!is.null(summary)) {
   #   return(regression()$fit.summary)
#    }
    
#  })
  
})