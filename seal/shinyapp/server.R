##server.R
library(shiny)
source("RW.R")

shinyServer(function(input, output) {
  
    
######1D SIMPLE RANDOM WALK  

  plotParamsB <- reactiveValues(stepsB = 1, probB=0.5, numbB=1, typesInputB = NA) 
    observeEvent(input$runSimB, {
                plotParamsB$stepsB <- input$stepsB
                plotParamsB$probB <- input$probB 
                plotParamsB$numbB <- input$numbB
                plotParamsB$typesInputB <- OneDimBrownianRandWalk(plotParamsB$stepsB,
                                                                  plotParamsB$probB) 
    })
  
  output$plotB <- renderPlot({    
    
    #### plot(xx$typesInputA ,type="l") 
    if (is.na(plotParamsB$typesInputB )) return(NULL)
    multiWalk(function(x) OneDimSimpleRandWalk(x, plotParamsB$probB), plotParamsB$stepsB, plotParamsB$numbB)     
    #plot(plotParamsB$typesInputB, type="l")     
    })

######1D BROWNIAN RANDOM WALK  
  
  plotParamsC <- reactiveValues(stepsC = 1, meanC = 0, sdC = 1, numbC=1, typesInputC = NA) 
    observeEvent(input$runSimC, {
              plotParamsC$meanC <- input$meanC
              plotParamsC$sdC <- input$sdC
              plotParamsC$stepsC <- input$stepsC
              plotParamsC$numbC <- input$numbC
              plotParamsC$typesInputC <- OneDimBrownianRandWalk(plotParamsC$stepsC,
                                         plotParamsC$meanC, plotParamsC$sdC)        
    })
  output$plotC <- renderPlot({    
    
    #### plot(xx$typesInputA ,type="l") 
    if (is.na(plotParamsC$typesInputC )) return(NULL)
    multiWalk(function(x) OneDimBrownianRandWalk(x,  plotParamsC$meanC, plotParamsC$sdC), plotParamsC$stepsC, plotParamsC$numbC)     
    })
  
######2D SIMPLE RANDOM WALK  

  plotParamsD <- reactiveValues(stepsD = 1, xporbD = 0.5, yprobD = 0.5,typesInputD = NA) 
    observeEvent(input$runSimD, {
            plotParamsD$stepsD <- input$stepsD
            plotParamsD$xprobD <- input$xprobD
            plotParamsD$yporbD <- input$yporbD
            plotParamsD$numbD <- input$numbD
            plotParamsD$typesInputD <- TwoDimSimpleRandWalk(plotParamsD$stepsD, 
                                                            plotParamsD$xprobD, plotParamsD$yporbD)        
})

  output$plotD <- renderPlot({    
  
  #### plot(xx$typesInputA ,type="l") 
    if (is.na(plotParamsD$typesInputD )) return(NULL)
    multiWalk(function(x) TwoDimSimpleRandWalk(x,  plotParamsD$xprobD, plotParamsD$yporbD), plotParamsD$stepsD, plotParamsD$numbD)     
  })

######2D BROWNIAN RANDOM WALK  

  plotParamsE <- reactiveValues(stepsE = 1, XmeanE = 0, XsdE =  1, YmeanE = 0, YsdE = 1, typesInputE = NA) 
    observeEvent(input$runSimE, {
           plotParamsE$stepsE <- input$stepsE
           plotParamsE$XmeanE <- input$XmeanE
           plotParamsE$XsdE <- input$XsdE
           plotParamsE$YmeanE <- input$YmeanE
           plotParamsE$YsdE <- input$YsdE
           plotParamsE$numbE <- input$numbE
           plotParamsE$typesInputE <- TwoDimBrownianRandWalk(plotParamsE$stepsE, 
                                            plotParamsE$XmeanE, plotParamsE$XsdE, 
                                            plotParamsE$YmeanE, plotParamsE$YsdE)        
})

output$plotE <- renderPlot({    

  #### plot(xx$typesInputA ,type="l") 
  if (is.na(plotParamsE$typesInputE )) return(NULL)
  multiWalk(function(x) TwoDimBrownianRandWalk(x,   plotParamsE$XmeanE, plotParamsE$XsdE, 
                                               plotParamsE$YmeanE, plotParamsE$YsdE), 
                                              plotParamsE$stepsE, plotParamsE$numbE)     
})



######2D Random Walk with Drift
plotParamsG <- reactiveValues(xinitG = 1, yinitG = 1, deltaG = 0.0065,
                                sigmaG = 0.005, typesInputG = NA)
  observeEvent(input$runSimG, {
               plotParamsG$xinitG <- input$xinitG
               plotParamsG$yinitG <- input$yinitG
               plotParamsG$numbG <- input$numbG
               plotParamsG$stepsG <- 0
               plotParamsG$deltaG <- 0.0065
               plotParamsG$sigmaG <- 0.005
               
  plotParamsG$typesInputG <- TwoDimRandWalkDrift(plotParamsG$xinitG, plotParamsG$yinitG,
                                                 plotParamsG$deltaG, plotParamsG$sigmaG)
  })

output$plotG <- renderPlot({
  
  if (is.na(plotParamsG$typesInputG)) return(NULL)
  multiWalk(function(x) TwoDimRandWalkDrift(plotParamsG$xinitG, plotParamsG$yinitG, 
                                            plotParamsG$deltaG, plotParamsG$sigmaG), 
                                            plotParamsG$stepsG, plotParamsG$numbG)     
})



######Great Circle RANDOM WALK  

plotParamsF <- reactiveValues(stepsF = 0,latBeginF = 0, lonBeginF = 0, 
                              latEndF = 1, lonEndF = 1,
                              deltaG = 0.0065, sigmaG = 0.005,typesInputF = NA) 
  observeEvent(input$runSimF, {
              plotParamsF$latBeginF <- input$latBeginF
               plotParamsF$lonBeginF <- input$lonBeginF
               plotParamsF$latEndF <- input$latEndF
               plotParamsF$lonEndF <- input$lonEndF
               plotParamsF$numbF <- input$numbF
               plotParamsF$stepsF <- 0
               plotParamsF$deltaF <- 0.0065
               plotParamsF$sigmaF <- 0.005               
    
  plotParamsF$typesInputF <-GCRandomWalk( plotParamsF$stepsF,
                                          plotParamsF$latBeginF, plotParamsF$lonBeginF,
                                          plotParamsF$latEndF, plotParamsF$lonEndF,
                                          plotParamsF$deltaF, plotParamsF$sigmaF )        
})

output$plotF <- renderPlot({    
  
  #### plot(xx$typesInputA ,type="l") 
  if (is.na(plotParamsF$typesInputF )) return(NULL)
           lonRange = abs(plotParamsF$lonBeginF - plotParamsF$lonEndF)
  if (lonRange < 45) {
           xlimF = c(plotParamsF$lonBeginF, plotParamsF$lonBeginF + 45 )
  } else xlimF = c(plotParamsF$lonBeginF,  plotParamsF$lonEndF)
  
  multiWalk(function(x) GCRandomWalk(plotParamsF$stepsF,
                                     plotParamsF$latBeginF, plotParamsF$lonBeginF,
                                     plotParamsF$latEndF, plotParamsF$lonEndF,
                                     plotParamsF$deltaF, plotParamsF$sigmaF), plotParamsF$stepsF, plotParamsF$numbF)     
})

})


