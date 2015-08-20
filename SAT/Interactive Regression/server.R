library(shiny)

#getwd()
#setwd("~/DynDocs/SAT/Interactive Regression")
load("data/satDF.rda")

shinyServer(function(input, output) {
  *
  satDF$halves = cut(satDF$frac, breaks = 3)  
  
  lm_list = list()
  for(i in 1:3){
    lm_list[[i]] = lm(satDF$sat[which(satDF$halves==levels(satDF$halves)[i])]~ satDF$salary[which(satDF$halves==levels(satDF$halves)[i])], satDF)
  }
  
  party = list()
  for(i in 1:3){
  party[[i]] = satDF[which(satDF$halves==levels(satDF$halves)[i]) , ]
}


regression <- reactive({

    if (input$model == "Simple regression") {
      fit.res <- lm(satDF$sat~ satDF$salary, satDF)
    } else if (input$model == "Controlling for frac") {
      fit.res[[1]] = lm(satDF$sat ~ satDF$salary, satDF[10-30, ])
      fit.res[[2]] = lm(satDF$sat ~ satDF$salary, satDF[10-30, ])
    } else if (input$model == "First Half of Data") {
      fit.res = lm_list[[1]] 
    } else if (input$model == "Second Half of Data") {
      fit.res = lm_list[[2]]
    } else if (input$model == "Third Half of Data") {
      fit.res = lm_list[[3]]
    } 
    
# Get the model summary
    if (is.null(fit.res)) {
      fit.summary = NULL
   }  else {
      fit.summary = summary(fit.res)
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
    } else if (input$model == "First Half of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[1]]$salary, party[[1]]$sat, pch=16, cex=1.2, col="red")      
      abline(lm_list[[1]]$coefficients, lwd=3, col= "red") 
    } else if (input$model == "Second Half of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[2]]$salary, party[[2]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[2]]$coefficients, lwd=3, col= "red")
    } else if (input$model == "Third Half of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[3]]$salary, party[[3]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[3]]$coefficients, lwd=3, col= "red")
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