#server.R for shinyAR
library(shiny)

AR1 = function(c , rho, sigma){
  noise = rnorm(400, 0 , sigma)
  AR = c()
  AR[1] = c + noise[1]
  for(i in 2:length(noise)){
    AR[i] = c + rho * AR[i-1] + noise[i]
  }
return(AR)
}

plotAR = function(c,rho, sigma){
  par(mfrow = c(3,3))
  a = replicate(9, plot(AR1(c,rho, sigma), type = "l" , main = "AR curve"))
}


shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
      
   plotAR(input$c,input$rho,input$sigma)
  }
)
}
)