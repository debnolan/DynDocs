library("shiny")

#load functions for computing/vizualizing permutations tests
source("helpers.R")

#load HTML interactive choices
load("htmlChoices.rda")

#load data
load("data/drugs.RData")

shinyServer(
  function(input, output) {
    #select drug data set using a reactive widget
    dataSet <- reactive({ 
      drugData[[input$var]]
    })
    output$intro <- renderText({paste(intros[[input$var]], collapse = "")})
    #vizualize selected data with a table
    output$data <- renderTable({
      data.frame(dataSet())
    })
    #display the mean for both populations
    output$mean <- renderTable({
      data.frame("mean1" = mean(dataSet()[,1], na.rm = TRUE), 
                 "mean2" = mean(dataSet()[,2]))
    })
    #allow user to select repetitions of sample from the selected data
    #plot a distribution of the means of the sample populations
    #compute p-Value for the sample set of population means
    output$permPlot <- renderPlot({
      args <- list(NRep = input$NRep1,
                   pop1 = na.omit(dataSet()[,1]), pop2 = dataSet()[,2])
      do.call(plotPerm, args)
    })
    #plot convergence of p-values as repetitions increase for selected data
    #demonstrate necessary number of repetitions for accurate p-value
    output$plot <- renderPlot({
      args <- list(min = input$range[1], max = input$range[2],
                   pop1 = na.omit(dataSet()[,1]), pop2 = dataSet()[,2], big = input$var)
      
      do.call(pValPlot, args)
    })
    #user inputs threshold value
    #vizualizes the dichotomization of the selected data with chosen threshold
    output$dataDich <- renderTable({
      dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2, names = input$var)
    })
    #provides the sum of original dichotomization for comparison
    output$sum <- renderTable({
      data.frame("dichotomization 1" = sum(dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2)$dich1, 
                                           na.rm = TRUE),
                 "dichotomization 2" = sum(dichData(dataSet()[,1], dataSet()[,2], thresh = input$thresh2)$dich2))
    })
    #provides the median of the drug variable as a possible threshold value
    output$median <- renderText({
      paste("Median of Drug Data = ", 
            as.character(median(c(dataSet()[,1], dataSet()[,2]), na.rm = TRUE)))
    })
    
    output$threshold <- renderText({paste(choices[[input$var]], collapse = "")})
    #allow user to select repetitions of sample from the dichotomized data
    #plot a distribution of the sums of the sample populations
    #compute p-Value for the sample set of population sums
    output$dichPlot <- renderPlot({
      args <- list(NRep = input$NRep2, thresh = input$thresh2,
                   pop1 = na.omit(dataSet()[,1]), pop2 = dataSet()[,2])
      do.call(dichPlot, args)
    })
    #vizualize the ranks of the selected data in a table
    output$rank <- renderTable({
      rankData(dataSet()[,1], dataSet()[,2], names = input$var)
    })
    #provide the ranked sum of the drug variable for comparison
    output$rankSum <- renderText({
      paste("Ranked Sum of",
            paste(input$var,
                  paste("=",
                        sum(
                          rankData(
                            pop1 = dataSet()[,1], pop2 = dataSet()[,2],
                            names = input$var)[,2], na.rm = TRUE)
                  )))
    })
    #allow user to select repetitions of sample from the ranked data
    #plot a distribution of the ranked-sums of the sample populations
    #compute p-Value for the sample set of population ranked-sums
    output$wilcox <- renderPlot({
      args <- list(NRep = input$NRep3,
                   pop1 = na.omit(dataSet()[,1]), pop2 = dataSet()[,2])
      do.call(wilPlot, args)
    })
  }
)