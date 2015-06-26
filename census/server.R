library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output expressions defined 
  # below then all used the value computed from this expression
  seatsEx <- reactiveValues(cts = list(c(1,1,1)))
    
  observeEvent(input$A, {
      n = length(seatsEx$cts)
      newVals = c(input$A, input$M, input$W)
      if (sum(newVals) != 6) {
        return() }
      else {
        seatsEx$cts[[n+1]] = newVals
      }  
      
    })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    n = length(seatsEx$cts)
    #if (sum(seatsEx$cts[[n]]) != 6) return()
    
    plot( c(0,0,0) ~ (1:3), type = "n", xlim = c(0.5, 3.5),
          ylim = c(0, 1400), log = "y")
  
    lapply(seatsEx$cts, function(cts) {
      points(cts ~ (1:3), type ="l", col= rainbow(10)[n])
    })
   
  })
    
})
