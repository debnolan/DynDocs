library(shiny)

#getwd()
#setwd("~/DynDocs/SAT/Interactive Regression")
load("data/satDF.rda")

shinyServer(function(input, output) {  
  
  nums = list(1:25, 26:50, 1:15, 20:30, 35:50)

  lm_list = list()
  for(i in 1:5){  
  lm_list[[i]] = lm(satDF$sat ~ satDF$salary, subset = order(satDF$frac)[nums[[i]]])
  }
  
  party = list()
  for(i in 1:5){
  party[[i]] = satDF[order(satDF$frac)[nums[[i]]], ]
}

regression = reactive({

    if (input$model == "Simple regression") {
      fit.res = lm(satDF$sat~ satDF$salary, satDF)
    } else if (input$model == "Controlling for frac") {
      fit.res = list()
      fit.res[[1]] = lm_list[[3]]
      fit.res[[2]] = lm_list[[4]]
      fit.res[[3]] = lm_list[[5]]
    } else if (input$model == "First Half of Data") {
      fit.res = lm_list[[1]] 
    } else if (input$model == "Second Half of Data") {
      fit.res = lm_list[[2]]
    } 
    
# Get the model summary
    if (is.null(fit.res)) {
      fit.summary = NULL
   }  else {
     if (class(fit.res) == "list") {
      fit.summary = list()
      fit.summary[[1]] = summary(fit.res[[1]])
      fit.summary[[2]] = summary(fit.res[[2]])
      fit.summary[[3]] = summary(fit.res[[3]])    
     } else {
      fit.summary = summary(fit.res)  
     }
    }

return(list(fit.res=fit.res, fit.summary=fit.summary))

  })
  
  #---------------------------------------------------------------------------
  # Plot a scatter of the data with regression lines corresponding to the model
  output$reg.plot = renderPlot({         
  
    x = satDF$salary
    y = satDF$sat
    g = satDF$frac
    coefs = regression()$fit.res$coefficients
          
  plot(x, y, pch=16, type= "n", cex=1.2, bty="n",xlim=c(25, 50.5), main= "The Effect of Teacher Salary on SAT scores", xlab="Average Teacher Salary (in thousands of dollars)", ylab="Average Total SAT Scores")
  
  if (input$model == "Simple regression") {
    points(x, y, pch=16, cex=1.2, col="black")
      abline(coefs, lwd=3, col= "red")
    } else if (input$model == "Controlling for frac") {
      points(x, y, pch=16, cex=1.2, col="black")
      abline(lm_list[[3]]$coefficients, lwd=3, col= "blue")
      abline(lm_list[[4]]$coefficients, lwd=3, col= "gold")
      abline(lm_list[[5]]$coefficients, lwd=3, col= "red")
      points(party[[3]]$salary, party[[3]]$sat, pch=16, cex=1.2, col="blue")
      points(party[[4]]$salary, party[[4]]$sat, pch=16, cex=1.2, col="gold")
      points(party[[5]]$salary, party[[5]]$sat, pch=16, cex=1.2, col="red")
    } else if (input$model == "First Half of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[1]]$salary, party[[1]]$sat, pch=16, cex=1.2, col="red")      
      abline(lm_list[[1]]$coefficients, lwd=3, col= "red") 
    } else if (input$model == "Second Half of Data") {
      points(x, y, pch=16, cex=1.2, col="black")
      points(party[[2]]$salary, party[[2]]$sat, pch=16, cex=1.2, col="red")
      abline(lm_list[[2]]$coefficients, lwd=3, col= "red")
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

##Credit: some ideas and code were adapted from mwaskom at https://github.com/mwaskom/ShinyApps/blob/master/multi_regression/server.R to illustrate this example. 