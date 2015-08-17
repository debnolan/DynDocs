# server.R
source("helper.R")

shinyServer(function(input, output) {
  
  output$plotQQ <- renderPlot({
    
    if(input$NQQ){
      plotDataNormQ()
    }
    if (input$GQQ) {
      plotDataNormQ()
      plotDataGammaQ()
      legend(x= 12,y = 6, 
            lty=c(1,1), 
            lwd=c(2.5,2.5), col=c("red","blue"), pch = 21,
            legend = c("Normal QQ", "Gamma QQ"))
      } 
  })
  
  
  data <- reactive({
    
    input$runSim
    
    isolate({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)
      list(data = dist(input$n), name = input$dist, n = input$n)
    })
  })
    ##add if case to account for alpha and beta parameters - above and below

    output$plot1 <- renderPlot({
      
      if (input$cdf) {
        plot(ecdf(data()$data),
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
      }
      else {
        hist(data()$data, 
             main=paste('r', data()$name, '(', data()$n, ')', sep=''))
      }
    })
    

    
    plotNum <- reactiveValues(obs = 1)
    
    observeEvent( input$plotDistr, {
      plotNum$obs = input$num
    })
    
    output$plotGD <- renderPlot({  
      if (input$Gcdf){
        plotCDF(n=plotNum$obs)
      }
      else{
        plotDistr(n=plotNum$obs)
      }
    })
    
    
    alpha <- reactiveValues(a = .5)
    
    observeEvent( input$plotAlpha, {
      alpha$a = input$alphalvl
    })
    
    output$alph <- renderPlot({  
      if(alpha$a == "0.5"){
        graphGamma()
      }
      if(alpha$a == "1"){
        graphGamma1()
      }
      if(alpha$a == "1.5"){
        graphGamma1half()
      }
      if(alpha$a == "2"){
        graphGamma2()
      }
      if(alpha$a == "3"){
        graphGamma3()
      }
      if(alpha$a == "4"){
        graphGamma4()
      }
    })
    
    beta <- reactiveValues(b = .5)
    
    
    observeEvent( input$plotBeta, {
      beta$b = input$betalvl
    })
    
    output$beta <- renderPlot({  
      x <- seq(0, 7, by=.001)
      if(beta$b == "0.5"){
        graphbGamma()
      }
      if(beta$b == "1"){
        graphbGamma1()
      }
      if(beta$b == "1.5"){
        graphbGamma1half()
      }
      if(beta$b == "2"){
        graphbGamma2()
      }
      if(beta$b == "3"){
        graphbGamma3()
      }
      if(beta$b == "4"){
        graphbGamma4()
      }
    })
    
    
    
    plotParams <- reactiveValues(alpha = 1, beta = 1)
    
    observeEvent( input$makePlot, {
      plotParams$alpha = input$alpha
      plotParams$beta = input$beta
    })
    
    output$plot2 <- renderPlot({  
      graphGamma(alpha = plotParams$alpha, beta = plotParams$beta)
    })
})

