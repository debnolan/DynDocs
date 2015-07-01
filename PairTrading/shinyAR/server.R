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
      if(input$rho > 1 | input$rho < -1){
        list(err = TRUE)
      }
        else{
      list(const = input$c, rho = input$rho, sigma = input$sigma, err = FALSE)}
    })
  })
  
  output$warn <- renderText({
  if(!data()$err){""
  }else{
    "&rho; must be value between 1 and -1."
    }
    })
  output$plot <- renderPlot({
    if(data()$err){ return(NULL)} 
      else{
      plotAR(data()$const,data()$rho,data()$sigma)}
  }
)
}
)