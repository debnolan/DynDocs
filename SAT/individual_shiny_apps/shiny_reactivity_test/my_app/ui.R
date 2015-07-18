library(shiny)
shinyUI(fluidPage(
 
  # Application title
  titlePanel("SAT Scores"),
  
  # inputs
    fluidRow(
      column(width = 4, 
             # variable names might need change
             selectInput("x_1", "Choose an x variable:", 
                         choices = list("Expenditure Per Pupil" = "expend", 
                                     "Student-Teacher Ratio" = "ratio", 
                                     "Teacher Salary" = "salary", 
                                     "Eligible Student Fraction" = "frac"), 
                         selected = "salary"),
             selectInput("y_1", "Choose a y variable", 
                         choices = list("SAT Verbal Score" = "verbal",
                                     "SAT Math Score" = "math", 
                                     "SAT Total Score" = "sat"), 
                         selected = "sat")       
             )
      ),
      
#       column(width = 4, 
#              offset = 3, 
#              selectInput("x_2", "Choose an x variable:", 
#                          choices = list("Expenditure Per Pupil" = "Expenditure", 
#                                         "Student-Teacher Ratio" = "Student-Teacher Ratio", 
#                                         "Teacher Salary" = "Teacher Salary", 
#                                         "Eligible Student Fraction" = "Eligible Student Fraction"), 
#                          selected = "Teacher Salary"),
#              selectInput("y_2", "Choose a y variable", 
#                          choices = list("SAT Verbal Score" = "SAT Verbal Score",
#                                         "SAT Math Score" = "SAT Math Score", 
#                                         "SAT Total Score" = "SAT Total Score"), 
#                          selected = "SAT Total Score")
#              )
#       ),
#     
    fluidRow(
      column(width = 6, 
             # h3(textOutput("caption")),
             plotOutput("scatterPlot_1")
      )
#       ,
#       column(width = 6, 
#              plotOutput("scatterPlot_2") 
#       )
     )
  )
)
