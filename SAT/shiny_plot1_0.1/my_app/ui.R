library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.1")
load("satDF_new.rda")
shinyUI(fluidPage(
  # Application title
  titlePanel("SAT Scores"),
  
  # inputs
  sidebarLayout(
    sidebarPanel(
      
      selectInput("x", "Choose an x variable:", choices = names(satDF_new)[2:5], 
                  selected = names(satDF_new)[2]),
      selectInput("y", "Choose a y variable", choices = names(satDF_new)[6:8], 
                  selected = names(satDF_new)[8]) 
    ),
    
    # Show the plots
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("scatterPlot")
    )
  )
))