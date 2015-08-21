require(shiny)

load("data/mnist.RData")

shinyServer(
  function(input, output, session) {
    output$processedVar1 <- renderPlot({
      args1 <- switch(input$var1,
                      "zero" = SamplesProcessed[[1]],
                      "one" = SamplesProcessed[[2]],
                      "two" = SamplesProcessed[[3]],
                      "three" = SamplesProcessed[[4]],
                      "four" = SamplesProcessed[[5]],
                      "five" = SamplesProcessed[[6]],
                      "six" = SamplesProcessed[[7]],
                      "seven" = SamplesProcessed[[8]],
                      "eight" = SamplesProcessed[[9]],
                      "nine" = SamplesProcessed[[10]])
      image(args1)
    })
    output$processedVar2 <- renderPlot({
      args1 <- switch(input$var1,
                      "zero" = SamplesProcessed[[1]],
                      "one" = SamplesProcessed[[2]],
                      "two" = SamplesProcessed[[3]],
                      "three" = SamplesProcessed[[4]],
                      "four" = SamplesProcessed[[5]],
                      "five" = SamplesProcessed[[6]],
                      "six" = SamplesProcessed[[7]],
                      "seven" = SamplesProcessed[[8]],
                      "eight" = SamplesProcessed[[9]],
                      "nine" = SamplesProcessed[[10]])
      args2 <- switch(input$var2,
                     "zero" = lapply(trainMean,function(x) matrix(x,nrow=28))[[1]],
                     "one" = lapply(trainMean,function(x) matrix(x,nrow=28))[[2]],
                     "two" = lapply(trainMean,function(x) matrix(x,nrow=28))[[3]],
                     "three" = lapply(trainMean,function(x) matrix(x,nrow=28))[[4]],
                     "four" = lapply(trainMean,function(x) matrix(x,nrow=28))[[5]],
                     "five" = lapply(trainMean,function(x) matrix(x,nrow=28))[[6]],
                     "six" = lapply(trainMean,function(x) matrix(x,nrow=28))[[7]],
                     "seven" = lapply(trainMean,function(x) matrix(x,nrow=28))[[8]],
                     "eight" = lapply(trainMean,function(x) matrix(x,nrow=28))[[9]],
                     "nine" = lapply(trainMean,function(x) matrix(x,nrow=28))[[10]])
      overlap(args2,args1)
    })
   
    output$processedVar3 <- renderUI({
      args3 <- switch(input$var3,
                      "zero" = SamplesProcessed[[1]],
                      "one" = SamplesProcessed[[2]],
                      "two" = SamplesProcessed[[3]],
                      "three" = SamplesProcessed[[4]],
                      "four" = SamplesProcessed[[5]],
                      "five" = SamplesProcessed[[6]],
                      "six" = SamplesProcessed[[7]],
                      "seven" = SamplesProcessed[[8]],
                      "eight" = SamplesProcessed[[9]],
                      "nine" = SamplesProcessed[[10]])
      
      forZero = paste("The digit matches ",round(percentageComparison(args3)[[1]],2),"% to zero")
      forOne = paste("The digit matches ",round(percentageComparison(args3)[[2]],2),"% to one")
      forTwo = paste("The digit matches ",round(percentageComparison(args3)[[3]],2),"% to two")
      forThree = paste("The digit matches ",round(percentageComparison(args3)[[4]],2),"% to three")
      forFour = paste("The digit matches ",round(percentageComparison(args3)[[5]],2),"% to four")
      forFive = paste("The digit matches ",round(percentageComparison(args3)[[6]],2),"% to five")
      forSix = paste("The digit matches ",round(percentageComparison(args3)[[7]],2),"% to six")
      forSeven = paste("The digit matches ",round(percentageComparison(args3)[[8]],2),"% to seven")
      forEight = paste("The digit matches ",round(percentageComparison(args3)[[9]],2),"% to eight")
      forNine = paste("The digit matches ",round(percentageComparison(args3)[[10]],2),"% to nine")
      
      result = paste("Your number is",naiveBayes(args3),"by Naive Bayes Classifier")
      
      HTML(paste("","",forZero,forOne,forTwo,forThree,forFour,forFive,forSix,forSeven,forEight,forNine,
                 "","",result,sep = '<br/>'))
    })
      
    output$processedVar4 <- renderPlot({
      args3 <- switch(input$var3,
                      "zero" = SamplesProcessed[[1]],
                      "one" = SamplesProcessed[[2]],
                      "two" = SamplesProcessed[[3]],
                      "three" = SamplesProcessed[[4]],
                      "four" = SamplesProcessed[[5]],
                      "five" = SamplesProcessed[[6]],
                      "six" = SamplesProcessed[[7]],
                      "seven" = SamplesProcessed[[8]],
                      "eight" = SamplesProcessed[[9]],
                      "nine" = SamplesProcessed[[10]])
      accuracyGraph(args3)
    })
    
    output$processedVar5 <- renderPlot({
      
      #jsonParserImage(input$var5) 
      
      image(jsonParser(input$var5))
  
    })
    
    output$processedVar6 <- renderUI({
      
      result = paste("Your digit is ",naiveBayes(jsonParser(input$var5)),"by Naive Bayes Classifier")
      
      HTML(result)
    })
    
    output$processedVar7 <- renderPlot({
      
      #jasonParserGraph(input$var5)
      accuracyGraph(jsonParser(input$var5))
      
    })
    
  }
)


