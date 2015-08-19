library(shiny)
library(RColorBrewer)
library(ggplot2)

#getwd()
#setwd("~/DynDocs/SAT/Interactive Regression")
load("data/satDF.rda")

shinyServer(function(input, output) {
  satDF$fourths = cut(satDF$frac, breaks = quantile(satDF$frac, probs= seq(0,1,5)))
  
  lm_list = list()
  for(i in 1:4){
  lm_list[[i]] = lm(satDF$sat[which(satDF$fourths==levels(satDF$fourths)[i])]~ satDF$salary[which(satDF$fourths==levels(satDF$fourths)[i])], satDF)
  }

  party = list()
  for(i in 1:4){
  party[[i]] = satDF[which(satDF$fourths==levels(satDF$fourths)[i]) , ]
}

regression <- reactive({

    if (input$model == "Simple regression") {
      fit.res <- lm(satDF$sat~ satDF$salary, satDF)
    } else if (input$model == "Controlling for frac") {
      fit.res <- lm(satDF$sat~ satDF$salary + satDF$frac, satDF)
    } else if (input$model == "First Fourth of Data") {
      fit.res <-lm_list[[1]] 
    } else if (input$model == "Second Fourth of Data") {
      fit.res <- lm_list[[2]]
    } else if (input$model == "Third Fourth of Data") {
      fit.res <- lm_list[[3]]
    } else if (input$model == "Last Fourth of Data") {
      fit.res <- lm_list[[4]]
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
  
    x = satDF$salary
    y = satDF$sat
    g = satDF$frac
    coefs = regression()$fit.res$coefficients
          
  plot(x, y, pch=16, type= "n", cex=1.2, bty="n", main= "The Effect of Teacher Salary on SAT scores", xlab="Average Teacher Salary (in thousands of dollars)", ylab="Average Total SAT Scores")
      
  
  if (input$model == "Simple regression") {
    points(x, y, pch=16, cex=1.2, col="black")
      abline(coefs, lwd=3, col= "red")
    } else if (input$model == "Controlling for frac") {
      points(x, y, pch=16, cex=1.2, col="black")
      abline(a = coefs[1], b = coefs[2], lwd=3, col= "red") 
    } else if (input$model == "First Fourth of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[1]]$salary, party[[1]]$sat, pch=16, cex=1.2, col="red")      
      abline(lm_list[[1]]$coefficients, lwd=3, col= "red") 
    } else if (input$model == "Second Fourth of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[2]]$salary, party[[2]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[2]]$coefficients, lwd=3, col= "red")
    }  else if (input$model == "Third Fourth of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[3]]$salary, party[[3]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[3]]$coefficients, lwd=3, col= "red")
    }  else if (input$model == "Last Fourth of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[4]]$salary, party[[4]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[4]]$coefficients, lwd=3, col= "red")
    }   
  })
    

#---------------------------------------------------------------------------
  
#####################################

#Show the lm() summary for the 
  output$reg.summary <- renderPrint({
    
    summary <- regression()$fit.summary
    if (!is.null(summary)) {
      return(regression()$fit.summary)
    }
    
  })
  
})