library(shiny)
# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.3")
load("../rda_data/satDF_new.rda")
shinyUI(fluidPage(
  # Application title
  titlePanel("SAT Scores"),
  
  # inputs
    fluidRow(
      column(width = 4, 
             selectInput("x_1", "Choose an x variable:", 
                         choices = list("Expenditure Per Pupil" = "Expenditure", 
                                         "Student-Teacher Ratio" = "Student-Teacher Ratio", 
                                         "Teacher Salary" = "Teacher Salary", 
                                         "Eligible Student Fraction" = "Eligible Student Fraction"), 
                         selected = "Teacher Salary"),
             selectInput("y_1", "Choose a y variable", 
                         choices = list("SAT Verbal Score" = "SAT Verbal Score",
                                        "SAT Math Score" = "SAT Math Score", 
                                        "SAT Total Score" = "SAT Total Score"), 
                         selected = "SAT Total Score")       
             ),
      
      column(width = 4, 
             offset = 3, 
             selectInput("x_2", "Choose an x variable:", choices = names(satDF_new)[2:5], 
                         selected = names(satDF_new)[2]),
             selectInput("y_2", "Choose a y variable", choices = names(satDF_new)[6:8], 
                         selected = names(satDF_new)[8])
             )
      ),
    
    fluidRow(
      column(width = 6, 
             # h3(textOutput("caption")),
             plotOutput("scatterPlot_1")
      ),
      column(width = 6, 
             plotOutput("scatterPlot_2") 
      )
     )
  )
)
