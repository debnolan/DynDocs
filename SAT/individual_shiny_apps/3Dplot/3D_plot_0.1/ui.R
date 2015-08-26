#' Define UI for application that plots random 3d points
shinyUI(fluidPage(
  
  # Application title
  titlePanel("3-variable-satterplot"),
  
  # Sidebar with a slider input for number of points
  fluidRow(
    column(width = 3, 
          selectInput("x3d", "Choose an x variable:", 
                      choices = list("Expenditure Per Pupil (in 000's of dollars)" = "expend", 
                                     "Student-Teacher Ratio" = "ratio", 
                                     "Teacher Salary (in 000's of dollars)" = "salary", 
                                     "Eligible Student Fraction" = "frac"), 
                      selected = "salary"),
          selectInput("y3d", "Choose a y variable", 
                      choices = list("SAT Total Score" = "sat",
                                     "SAT Math Score" = "math", 
                                     "SAT Verbal Score" = "verbal"), 
                      selected = "sat"),
          selectInput("z3d", "Choose a z variable:", 
                      choices = list("Expenditure Per Pupil (in 000's of dollars)" = "expend", 
                                     "Student-Teacher Ratio" = "ratio", 
                                     "Teacher Salary (in 000's of dollars)" = "salary", 
                                     "Eligible Student Fraction" = "frac"), 
                      selected = "frac")
        ),
    column(width = 6,
           offset =1,
           # Show the generated 3d scatterplot
           webGLOutput("sctPlot")
    )
  )
))