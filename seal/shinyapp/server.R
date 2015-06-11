
##server.R
library(shiny)
#source("RW.R")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
      
    # Return the requested dataset
    typesInput <- reactive({
    input$types
    })
  # num.steps <- reactive({
  #  as.numeric(input$steps)
  #  })
  # Generate a random walk plot
    output$plot <- renderPlot({    
      num.steps <- as.numeric(input$steps)
      
      OneDimSimpleRandWalk <- function(num.steps, prob = 0.5) {
        return(cumsum(sample(c(1, -1), num.steps, TRUE)))
      }
      
      OneDimBrownianRandWalk <- function(num.steps, mean = 0, sd = 1) {
        return(cumsum(rnorm(num.steps, mean, sd)))
      }
      
      TwoDimSimpleRandWalk <- function(num.steps, x.prob = 0.5, y.prob = 0.5) {
        pos = matrix(0, ncol = 2, nrow = num.steps)
        dir = sample(c(1, 2), num.steps, TRUE)
        move = sample(c(1, -1), num.steps, TRUE)
        pos[cbind(1:num.steps(), dir)] = move
        pos = apply(pos, 2, cumsum)
        return(pos)
      }
      
      TwoDimBrownianRandWalk <- function(num.steps, mean = 0, sd = 1) {
        return(cbind(OneDimBrownianRandWalk(num.steps, mean, sd),
                     OneDimBrownianRandWalk(num.steps, mean, sd)))
      }
            
    plot(typesInput() ,type="l")     
    })
  
  #Download Random Walk Plot
  output$down <- downloadHandler(
    filename = function() {
      paste("Random_Walk", input$var, sep=".")
    },
    content = function(file){
      if(input$var == "pdf")
        pdf(file)
      else
        png(file)
      plot(typesInput(), type="l")
    dev.off()
    })  
})