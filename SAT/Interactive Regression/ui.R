library(shiny)

fig.width <- 600
fig.height <- 450

shinyUI(pageWithSidebar(
  
  headerPanel("SAT and Teachers' Salaries"), 
  sidebarPanel(
    div(p("Relate modeling choices to plots and summaries of the models")),
    
    div(selectInput("model",
                 strong("Linear model to evaluate"),
                  choices=c("Simple regression",
                            "Controlling for frac"))

    
    )
  ),
  
  mainPanel(
    div(plotOutput("reg.plot", width=fig.width, height=fig.height)),
    div(class="span7", verbatimTextOutput("reg.summary"))
  )
  
))