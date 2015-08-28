# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_table1_0.1/")
library(shiny)
library(ggplot2)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  load("show_data_app/data/satDF_pretty.rda")
  
  output$mytable1 = renderDataTable({
    satDF_pretty
  }, options = list(orderClasses = TRUE))
  
})