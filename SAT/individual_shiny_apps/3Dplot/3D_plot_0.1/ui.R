library(shiny)
library(shinyRGL)

#' Define UI for application that plots random 3d points
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("3-variable-satterplot"),
  
  # Sidebar with a slider input for number of points
  sidebarPanel(
    selectInput("x", "Choose an x variable:", 
                choices = list("Expenditure Per Pupil (in 000's of dollars)" = "expend", 
                               "Student-Teacher Ratio" = "ratio", 
                               "Teacher Salary (in 000's of dollars)" = "salary", 
                               "Eligible Student Fraction" = "frac"), 
                selected = "salary"),
    selectInput("y", "Choose a y variable", 
                choices = list("SAT Total Score" = "sat",
                               "SAT Math Score" = "math", 
                               "SAT Verbal Score" = "verbal"), 
                selected = "sat"),
    selectInput("z", "Choose a z variable:", 
                choices = list("Expenditure Per Pupil (in 000's of dollars)" = "expend", 
                               "Student-Teacher Ratio" = "ratio", 
                               "Teacher Salary (in 000's of dollars)" = "salary", 
                               "Eligible Student Fraction" = "frac"), 
                selected = "frac")
  ),
  # Show the generated 3d scatterplot
  mainPanel(
    webGLOutput("sctPlot")
  )
))