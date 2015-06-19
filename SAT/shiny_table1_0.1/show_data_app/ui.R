# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_table1_0.1/")
library(shiny)
library(DT)
# Define UI for miles per gallon application
shinyUI(fluidPage(
  fluidRow(column(width = 12, 
                    dataTableOutput("mytable1"))
    
  )
))
