##server.R
library(shiny)
source("RW.R")

shinyServer(function(input, output) {
  
  ######1D SIMPLE RANDOM WALK  
    if (FALSE) {
    # Return the requested dataset
    typesInput <- reactive({
      input$typesB
    })
    # num.steps <- reactive({
    #  as.numeric(input$steps)
    #  })
    # Generate a random walk plot
  }
  
  plotParamsB <- reactiveValues(stepsB = 1, typesInputB = NA)
  
  observeEvent(input$runSimB, {
    plotParamsB$stepsB <- input$stepsB
    
    plotParamsB$typesInputB <- OneDimSimpleRandWalk(plotParamsB$stepsB)     
    
    
  })
  
  output$plotB <- renderPlot({  
    
    if (FALSE){
      num.steps <- input$stepsB
      typesInput <- OneDimSimpleRandWalk(num.steps)     
      
      
    }
    
    #### plot(xx$typesInputA ,type="l") 
    if (is.na(plotParamsB$typesInputB )) return(NULL)
    plot(plotParamsB$typesInputB ,type="l")     
  })

  
  
######1D BROWNIAN RANDOM WALK  
  
  plotParamsC <- reactiveValues(stepsC = 1, meanC = 0, sdC = 1, typesInputC = NA) 
  observeEvent(input$runSimC, {
    plotParamsC$meanC <- input$meanC
    plotParamsC$sdC <- input$sdC
    plotParamsC$stepsC <- input$stepsC
    
    plotParamsC$typesInputC <- OneDimBrownianRandWalk(plotParamsC$stepsC,
                                                      plotParamsC$meanC, plotParamsC$sdC)        
  })
  
  output$plotC <- renderPlot({    
    
    #### plot(xx$typesInputA ,type="l") 
    if (is.na(plotParamsC$typesInputC )) return(NULL)
    plot(plotParamsC$typesInputC, type = "l")     
  })
  
######2D SIMPLE RANDOM WALK  
if (FALSE) {
  # Return the requested dataset
  typesInput <- reactive({
    input$typesD
  })
}

plotParamsD <- reactiveValues(stepsD = 1, typesInputD = NA) 
observeEvent(input$runSimD, {
  plotParamsD$stepsD <- input$stepsD
  plotParamsD$xprobD <- input$xprobD
  plotParamsD$yporbD <- input$yporbD
  
  plotParamsD$typesInputD <- TwoDimSimpleRandWalk(plotParamsD$stepsD, plotParamsD$xprobD, plotParamsD$yporbD)        
})

output$plotD <- renderPlot({    
  if (FALSE){
    num.steps <- input$stepsD
    x.prob <- input$xprobD
    y.porb <- input$yprobD
    typesInput <- TwoDimSimpleRandWalk(num.steps, x.prob, y.prob)        
  }
  
  #### plot(xx$typesInputA ,type="l") 
  if (is.na(plotParamsD$typesInputD )) return(NULL)
  plot(plotParamsD$typesInputD ,type="l")     
})


######2D BROWNIAN RANDOM WALK  
if (FALSE) {
  # Return the requested dataset
  typesInput <- reactive({
    input$typesE
  })
}

plotParamsE <- reactiveValues(stepsE = 1, typesInputE = NA) 
observeEvent(input$runSimE, {
  plotParamsE$stepsE <- input$stepsE
  plotParamsE$XmeanE <- input$XmeanE
  plotParamsE$XsdE <- input$XsdE
  plotParamsE$YmeanE <- input$YmeanE
  plotParamsE$YsdE <- input$YsdE
  
  
  plotParamsE$typesInputE <- TwoDimBrownianRandWalk(plotParamsE$stepsE, plotParamsE$XmeanE, plotParamsE$XsdE, plotParamsE$YmeanE, plotParamsE$YsdE)        
})

output$plotE <- renderPlot({    
  if (FALSE){
    num.steps <- input$stepsE
    x.mean <- input$XmeanE
    x.sd <- input$XsdE
    y.mean <- input$YmeanE
    y.sd <- input$YsdE
    typesInput <- TwoDimBrownianRandWalk(num.steps, x.mean, x.sd, y.mean, y.sd)        
  }
  
  #### plot(xx$typesInputA ,type="l") 
  if (is.na(plotParamsE$typesInputE )) return(NULL)
  plot(plotParamsE$typesInputE ,type="l")     
})


######Great Circle RANDOM WALK  

plotParamsF <- reactiveValues(lonBeginF = 0, lonEndF = 1, 
                              latBeginF = 0, latEndF = 1, typesInputF = NA) 
observeEvent(input$runSimF, {
  plotParamsF$lonBeginF <- input$lonBeginF
  plotParamsF$lonEndF <- input$lonEndF
  plotParamsF$latBeginF <- input$latBeginF
  plotParamsF$latEndF <- input$latEndF
  
  
  plotParamsF$typesInputF <-GCRandomWalk( plotParamsF$lonBeginF, plotParamsF$latBeginF,
                                          plotParamsF$lonEndF, plotParamsF$latEndF)        
})

output$plotF <- renderPlot({    
  
  #### plot(xx$typesInputA ,type="l") 
  if (is.na(plotParamsF$typesInputF )) return(NULL)
  lonRange = abs(plotParamsF$lonBeginF - plotParamsF$lonEndF)
  if (lonRange < 90) {
    xlimF = c(plotParamsF$lonBeginF, plotParamsF$lonBeginF + 90 )
  } else xlimF = c(plotParamsF$lonBeginF,  plotParamsF$lonEndF)
  
  plot(plotParamsF$typesInputF, xlim = xlimF, type = "l")     
})


  
})


