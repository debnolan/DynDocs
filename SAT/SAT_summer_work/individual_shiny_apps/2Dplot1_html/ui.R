library(shiny)
shinyUI(fluidPage(
 
  # Application title
  titlePanel("Scatter Plots and Correlation"),
  
  # inputs
    fluidRow(
      column(width = 4, 
             selectInput("x_1", "Choose an x variable:", 
                         choices = list("Expenditure Per Pupil" = "expend", 
                                         "Student-Teacher Ratio" = "ratio", 
                                         "Teacher Salary" = "salary", 
                                         "Eligible Student Fraction" = "frac"), 
                         selected = "salary"),
             selectInput("y_1", "Choose a y variable", 
                         choices = list("SAT Total Score" = "sat",
                                        "SAT Math Score" = "math", 
                                        "SAT Verbal Score" = "verbal"), 
                         selected = "sat")
             ),
      
      column(width = 4, 
             offset = 3, 
             selectInput("x_2", "Choose an x variable:", 
                         choices = list("Expenditure Per Pupil" = "expend", 
                                        "Student-Teacher Ratio" = "ratio", 
                                        "Teacher Salary" = "salary", 
                                        "Eligible Student Fraction" = "frac"), 
                         selected = "frac"),
             selectInput("y_2", "Choose a y variable", 
                         choices = list("SAT Total Score" = "sat",
                                        "SAT Math Score" = "math", 
                                        "SAT Verbal Score" = "verbal"), 
                         selected = "sat")
             )
      ),
    
    fluidRow(
      column(width = 6,
             div(style = "height:650px;", 
             plotOutput("sidebyside_1", 
                        hover = hoverOpts(id = "plot_hover_1", delayType = "throttle"))
      )
      ),
      column(width = 6, 
             div(style = "height:650px;",
             plotOutput("sidebyside_2", 
                        hover = hoverOpts(id = "plot_hover_2", delayType = "throttle"))
      )
      )
    ),
      
    fluidRow(
      column(width = 6, verbatimTextOutput("info_1")), 
      column(width = 6, verbatimTextOutput("info_2"))
    )
  )
)
