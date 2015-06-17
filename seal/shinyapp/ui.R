library(shiny)

# Define UI for application that plots random walk 
shinyUI(pageWithSidebar(
  
  # Application title
  titlePanel(title = h4("Random Walk Tutorial", align="center")),  
  
  # Sidebar with choices of Random Walk and specify the number of steps
  sidebarPanel(
#    radioButtons("types", "Choose a type of Random Walk:", 
    selectInput("types", "Choose a type of Random Walk:", 
                choices=c("1D Simple Rand Walk" = "OneDimSimpleRandWalk", 
                     "1D Brownian Rand Walk" = "OneDimBrownianRandWalk",
                     "2D Simple Rand Walk" = "TwoDimSimpleRandWalk", 
                     "2D Brownian Rand Walk" = "TwoDimBrownianRandWalk")),
    br(),
    
    sliderInput("steps", "Number of steps: ", 
                min = 1, max = 10000, value = 5000),
    
    br(),
    
    radioButtons(inputId = "var", label = "Select the file type", choices = list("png","pdf"))
),
  # Show a plot of the generated distribution
 mainPanel(
    plotOutput("plot"),
    downloadButton(outputId = "down", label = "Download the plot")
  )
))


