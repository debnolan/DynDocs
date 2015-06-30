#server.R for shinyAR
library(shiny)

AR1 = function(c , rho, sigma){
  noise = rnorm(300, 0 , sigma)
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