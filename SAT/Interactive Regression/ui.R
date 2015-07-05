library(shiny)

fig.width <- 600
fig.height <- 450

shinyUI(pageWithSidebar(
  
  headerPanel("SAT and Teachers' Salaries: The Truth Behind the Numbers"), 
  sidebarPanel(
    div(p("Relate modeling choices to plots and summaries of the models")),
    
    div(selectInput("model",
                 strong("Linear model to evaluate"),
                  choices=c("Simple regression",
                            "Controlling for frac",
                            "First Fifth of Data",
                            "Second Fifth of Data",
                            "Third Fifth of Data",
                            "Fourth Fifth of Data",
                            "Last Fifth of Data"))

    
    )
  ),
  
  mainPanel(
    div(plotOutput("reg.plot", width=fig.width, height=fig.height)),
    div(class="span7", verbatimTextOutput("reg.summary"))
  )
  
))