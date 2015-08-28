library(shiny)
options(rgl.useNULL=TRUE)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(ggvis)
library(shinyRGL)
library(rgl)
library(plyr)


# Define server logic required to draw a scatterplot
shinyServer(function(input, output) {
  load("data/satDF.rda")
  load("data/statepopulation.rda")
  
  output$text <- renderText({
    input$title
  })
  
  satDF_dataset <- reactive({
    # adding three columns to the original data frame: region, state population, and state abbreviation.
    data.frame(satDF, region = state.region, 
               population = statepopulation, state_abb = state.abb)
  })
  
  ###################### sidebyside_1
  output$sidebyside_1 <- renderPlot({
    satDF_1 <- satDF_dataset()
    
    x <- satDF_1[ , input$x_1]
    y <- satDF_1[ , input$y_1]
    
    dat <- data.frame(x, y, population = satDF_1$population, 
                      region = satDF_1$region, 
                      state_abb = satDF_1$state_abb)
    
    p <- ggplot(dat, aes(x, y, size = population, 
                         colour = region, label = state_abb))
    
    if(identical(x, satDF_1$expend)| identical(x, satDF_1$salary)){
      p + geom_point(alpha = 0.8) + 
        ggtitle(paste(input$y_1, "vs.", input$x_1)) +
        xlab(paste(input$x_1, "(in '000s of dollars)")) +
        ylab(input$y_1) + 
        scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
        theme_bw() +
        theme(plot.title = element_text(size = 25),
              legend.position = "top", 
              legend.background = element_rect(fill = "transparent", colour = "transparent"),
              legend.key = element_rect(colour = "transparent"))
    }else{
      p + geom_point(alpha = 0.8) + 
        ggtitle(paste(input$y_1, "vs.", input$x_1)) +
        xlab(input$x_1) +
        ylab(input$y_1) + 
        scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
        theme_bw() +
        theme(plot.title = element_text(size = 25),
              legend.position = "top", 
              legend.background = element_rect(fill = "transparent", colour = "transparent"),
              legend.key = element_rect(colour = "transparent"))
    }}, height = 500, width = 500)
  
  ###################### sidebyside_2
  output$sidebyside_2 <- renderPlot({
    
    satDF_2 <- satDF_dataset()
    
    x <- satDF_2[ , input$x_2]
    y <- satDF_2[ , input$y_2]
    
    dat <- data.frame(x, y, population = satDF_2$population, 
                 region = satDF_2$region, 
                 state_abb = satDF_2$state_abb)
    
    p <- ggplot(dat, aes(x, y, size = population, 
                         colour = region, label = state_abb))
    
    if(identical(x, satDF_2$expend)| identical(x, satDF_2$salary)){
      p + geom_point(alpha = 0.8) + 
        ggtitle(paste(input$y_2, "vs.", input$x_2)) +
        xlab(paste(input$x_2, "(in '000s of dollars)")) +
        ylab(input$y_2) + 
        scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
        theme_bw() +
        theme(plot.title = element_text(size = 25),
              legend.position = "top", 
              legend.background = element_rect(fill = "transparent", colour = "transparent"),
              legend.key = element_rect(colour = "transparent"))
    }else{
      p + geom_point(alpha = 0.8) + 
        ggtitle(paste(input$y_2, "vs.", input$x_2)) +
        xlab(input$x_2) +
        ylab(input$y_2) + 
        scale_size_continuous(range = c(3, 22), name = " population in 1994", labels = comma) +
        theme_bw() +
        theme(plot.title = element_text(size = 25),
              legend.position = "top", 
              legend.background = element_rect(fill = "transparent", colour = "transparent"),
              legend.key = element_rect(colour = "transparent"))
    }}, height = 500, width = 500)
  
  ###################### hover info
  output$info_1 <- renderPrint({
    
    nearPoints(satDF, input$plot_hover_1, xvar = input$x_1, yvar = input$y_1, threshold = 7)
  })
  
  output$info_2 <- renderPrint({
    nearPoints(satDF, input$plot_hover_2, xvar = input$x_2, yvar = input$y_2, threshold = 7)
  })
  
  ###################### data display
  output$mytable1 = renderTable({
    satdata <- satDF_dataset()
    # remove state name abbreviation column
    satdata <- satdata[-11]
    # convert all columns to character vectors
    satdata$State = state.name
    satdata$expend = sapply(satdata$expend, function(e) paste("$", as.character(e), sep = ""))
    satdata$salary = sapply(satdata$salary, function(e) paste("$", as.character(e), sep = ""))
    satdata$population= sapply(satdata$population, 
                             function(e) format(e, big.mark=",", scientific=FALSE))
    char_col = as.data.frame(apply(satdata[, c(3, 5, 6, 7, 8)], 2, as.character))
    satdata = data.frame(
      "State"= satdata$State, 
      "population" = satdata$population, 
      "salary" = satdata$salary, 
      "expend" = satdata$expend,
      "frac" = char_col$frac,
      "ratio" = char_col$ratio,
      "sat" = char_col$sat,
      "math" = char_col$math,
      "verbal" = char_col$verbal)
    satdata = rename(satdata, c("State" = "STATE",
                      "population" = "POPULATION",
                      "salary" = "TEACHER'S SALARY (IN '000S OF DOLLARS)",
                      "expend" = "EXPENDITURE PER PUPIL (IN '000S OF DOLLARS)",
                     "frac" = "ELIGIBLE STUDENT FRACTION (%)", 
                     "ratio" = "STUDENT-TEACHER RATIO", 
                     "sat" = "SAT TOTAL SCORE",
                     "math" = "SAT MATH SCORE", 
                     "verbal" = "SAT VERBAL SCORE"
                     ))
    satdata
  }, options = list(orderClasses = TRUE))
 
  
  
  ###################### regression 
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
  
   
  ###################### Simpson's Paradox tutorial 1
  count_rate_values <- reactive({
    success_rate_A_small = 0.93
    success_rate_A_large = 0.73
    success_rate_B_small = 0.87
    success_rate_B_large = 0.69
    
    # Treatment A values
    success_count_A_small = 
      round(success_rate_A_small * input$count_A_small, digits = 0)
    success_count_A_large = 
      round(success_rate_A_large * (350 - input$count_A_small), digits = 0)
    success_count_A_both = 
      round((success_count_A_small + success_count_A_large), digits = 0)
    success_rate_A_both =
      round((success_count_A_small + success_count_A_large)/ 350 * 100, digits = 0)
    
    # Treatment B values
    success_count_B_small = 
      round(success_rate_B_small * input$count_B_small, digits = 0)
    success_count_B_large = 
      round(success_rate_B_large * (350 - input$count_B_small), digits = 0)
    success_count_B_both = 
      round((success_count_B_small + success_count_B_large), digits = 0)
    success_rate_B_both = 
      round((success_count_B_small + success_count_B_large)/ 350 *100, digits = 0)
    
    countA = c(success_count_A_small, success_count_A_large, success_count_A_both)
    rateA = c(success_rate_A_small, success_rate_A_large, success_rate_A_both)
    countB = c(success_count_B_small, success_count_B_large, success_count_B_both)
    rateB = c(success_rate_B_small, success_rate_B_large, success_rate_B_both)
    
    as.data.frame(cbind(countA, rateA, countB, rateB))
    
  })
  
  cell_values <- reactive({
    df <- count_rate_values()
    # df <- apply(df, 2, as.character)
    cell11count = paste(df[1,1], "/", as.character(input$count_A_small), sep = "")
    cell21count = paste(df[2,1], "/", as.character(350 - input$count_A_small), sep = "")
    cell31count = paste(df[3,1], "/", "350", sep = "")
    cell11rate = paste(df[1,2] * 100, "%", sep = "")
    cell21rate = paste(df[2,2] * 100, "%", sep = "")
    cell31rate = paste(df[3,2], "%", sep = "")
    
    cell12count = paste(df[1,3], "/", as.character(input$count_B_small), sep = "")
    cell22count = paste(df[2,3], "/", as.character(350 - input$count_B_small), sep = "")
    cell32count = paste(df[3,3], "/", "350", sep = "")
    cell12rate = paste(df[1,4] * 100, "%", sep = "")
    cell22rate = paste(df[2,4] * 100, "%", sep = "")
    cell32rate = paste(df[3,4], "%", sep = "")
    
    df <- data.frame(
      cases = c("Small Stones", "Large Stones", "Both"),
      treatmentA = c(paste(cell11count, " ", "(", cell11rate, ")", sep = ""), 
                     paste(cell21count, " ", "(", cell21rate, ")", sep = ""), 
                     paste(cell31count, " ", "(", cell31rate, ")", sep = "")),
      treatmentB = c(paste(cell12count, " ", "(", cell12rate, ")", sep = ""), 
                     paste(cell22count, " ", "(", cell22rate, ")", sep = ""), 
                     paste(cell32count, " ", "(", cell32rate, ")", sep = "")),
      
      stringsAsFactors = FALSE)
    
    rename(df, c("cases"= "Severity",
                 "treatmentA" = "Treatment A", 
                 "treatmentB" = "Treatment B"))
  }) 
  
  
  # Show the values using an HTML table
  output$table <- renderTable({cell_values()})
  
  output$yesorno <- renderText({
    dfi<- count_rate_values()
    if(isTRUE(dfi[3,2] < dfi[3,4])){
      print("Simpson's Paradox: YES")
    }else{
      print("Simpson's Paradox: NO")
    }
  })  
  
  ###################### Simpson's Paradox tutorial 2
  
  count_rate_values2 <- reactive({
    success_rate_A_small2= input$success_rate_A_small2
    success_rate_A_large2= input$success_rate_A_large2 
    success_rate_B_small2= input$success_rate_B_small2
    success_rate_B_large2= input$success_rate_B_large2
    
    # Treatment A values
    success_count_A_small2 = 
      round(success_rate_A_small2* input$count_A_small2, digits = 0)
    success_count_A_large2 = 
      round(success_rate_A_large2 * (350 - input$count_A_small2), digits = 0)
    success_count_A_both2 = 
      round((success_count_A_small2 + success_count_A_large2), digits = 0)
    success_rate_A_both2 =
      round((success_count_A_small2 + success_count_A_large2)/ 350 * 100, digits = 0)
    
    # Treatment B values
    success_count_B_small2 = 
      round(success_rate_B_small2 * input$count_B_small2, digits = 0)
    success_count_B_large2 = 
      round(success_rate_B_large2 * (350 - input$count_B_small2), digits = 0)
    success_count_B_both2 = 
      round((success_count_B_small2 + success_count_B_large2), digits = 0)
    success_rate_B_both2 = 
      round((success_count_B_small2 + success_count_B_large2)/ 350 * 100, digits = 0)
    
    df2 <- as.data.frame(cbind(c(success_count_A_small2, success_count_A_large2, success_count_A_both2), 
                               c(success_rate_A_small2, success_rate_A_large2, success_rate_A_both2),
                               c(success_count_B_small2, success_count_B_large2, success_count_B_both2),
                               c(success_rate_B_small2, success_rate_B_large2, success_rate_B_both2)))
    
  })
  
  cell_values2 <- reactive({
    df2 <- count_rate_values2()
    #     df2 <- apply(df2, 2, as.character)
    cell11count2 = paste(df2[1,1], "/", as.character(input$count_A_small2), sep = "")
    cell21count2 = paste(df2[2,1], "/", as.character(350 - input$count_A_small2), sep = "")
    cell31count2 = paste(df2[3,1], "/", "350", sep = "")
    cell11rate2 = paste(df2[1,2] * 100, "%", sep = "")
    cell21rate2 = paste(df2[2,2] * 100, "%", sep = "")
    cell31rate2 = paste(df2[3,2], "%", sep = "")
    
    cell12count2 = paste(df2[1,3], "/", as.character(input$count_B_small2), sep = "")
    cell22count2 = paste(df2[2,3], "/", as.character(350 - input$count_B_small2), sep = "")
    cell32count2 = paste(df2[3,3], "/", "350", sep = "")
    cell12rate2 = paste(df2[1,4] * 100, "%", sep = "")
    cell22rate2 = paste(df2[2,4] * 100, "%", sep = "")
    cell32rate2 = paste(df2[3,4], "%", sep = "")
    
    df2 <- data.frame(
      cases2 = c("Small Stones", "Large Stones", "Both"),
      treatmentA2 = c(paste(cell11count2, " ", "(", cell11rate2, ")", sep = ""), 
                      paste(cell21count2, " ", "(", cell21rate2, ")", sep = ""), 
                      paste(cell31count2, " ", "(", cell31rate2, ")", sep = "")),
      treatmentB2 = c(paste(cell12count2, " ", "(", cell12rate2, ")", sep = ""), 
                      paste(cell22count2, " ", "(", cell22rate2, ")", sep = ""), 
                      paste(cell32count2, " ", "(", cell32rate2, ")", sep = "")),
      
      stringsAsFactors = FALSE)
    
    rename(df2, c("cases2"= "Severity",
                  "treatmentA2" = "Treatment A", 
                  "treatmentB2" = "Treatment B"))
  }) 
  
  
  # Show the values using an HTML table
  output$table2 <- renderTable({cell_values2()})
  
  output$yesorno2 <- renderText({
    dff<- count_rate_values2()
    paradox1 = (dff[1,2] < dff[1,4]) & (dff[2,2] < dff[2,4]) & (dff[3,2] > dff[3,4])
    paradox2 = (dff[1,2] > dff[1,4]) & (dff[2,2] > dff[2,4]) & (dff[3,2] < dff[3,4])
    if(isTRUE(paradox1)){
      print("Simpson's Paradox: YES")
    }else{
      if(isTRUE(paradox2)){
        print("Simpson's Paradox: YES")
      }else{
        print("Simpson's Paradox: NO")
      }
    }
  })
})