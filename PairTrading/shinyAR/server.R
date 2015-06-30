#server.R for shinyAR
library(shiny)

AR1 = function(const , rho, sigma, n = 300){
  noise = rnorm(n, 0 , sigma)
  AR = numeric(length = n)
  AR[1] = const + noise[1]
  for(i in 2:n){
    AR[i] = const + rho * AR[i-1] + noise[i]
  }
return(AR)
}

plotAR = function(const,rho, sigma){
  par(mfrow = c(3,3), mar = c(2,2,1,1))
  a = replicate(9, plot(AR1(const, rho, sigma), type = "l", main = ""))
}


shinyServer(function(input, output) {
  
  data <- reactive({
    
    input$runSim
    
    isolate({
      list(c = input$c, rho = input$rho, sigma = input$sigma)
    })
  })
  
  output$plot <- renderPlot({
      
   plotAR(data()$c,data()$rho,data()$sigma)
  }
)
}
)