library(shiny)
library(plyr)

# Define server logic for slider examples
shinyServer(function(input, output) {

  sliderValues <- reactive({
    # Treatment A values
    success_count_A_small = 
      round(input$success_rate_A_small * input$count_A_small, digits = 0)
    success_count_A_large = 
      round(input$success_rate_A_large * (350 - input$count_A_small), digits = 0)
    success_count_A_both = 
      round((success_count_A_small + success_count_A_large), digits = 0)
    success_rate_A_both =
      round((success_count_A_small + success_count_A_large)/ 350 * 100, digits = 0)
    # Treatment B values
    success_count_B_small = 
      round(input$success_rate_B_small * input$count_B_small, digits = 0)
    success_count_B_large = 
      round(input$success_rate_B_large * (350 - input$count_B_small), digits = 0)
    success_count_B_both = 
      round((success_count_B_small + success_count_B_large), digits = 0)
    success_rate_B_both = 
      round((success_count_B_small + success_count_B_large)/ 350 *100, digits = 0)
    
    cell11count = paste(as.character(success_count_A_small), "/", as.character(input$count_A_small), sep = "")
    cell21count = paste(as.character(success_count_A_large), "/", as.character(input$count_A_small), sep = "")
    cell31count = paste(as.character(success_count_A_both), "/", "350", sep = "")
    cell11rate = paste(input$success_rate_A_small * 100, "%", sep = "")
    cell21rate = paste(input$success_rate_A_large * 100, "%", sep = "")
    cell31rate = paste(as.character(success_rate_A_both), "%", sep = "")
    
    cell12count = paste(as.character(success_count_B_small), "/", as.character(input$count_B_small), sep = "")
    cell22count = paste(as.character(success_count_B_large), "/", as.character(input$count_B_small), sep = "")
    cell32count = paste(as.character(success_count_B_both), "/", "350", sep = "")
    cell12rate = paste(input$success_rate_B_small *100, "%", sep = "")
    cell22rate = paste(input$success_rate_B_large * 100, "%", sep = "")
    cell32rate = paste(as.character(success_rate_B_both), "%", sep = "")
     
    df <- data.frame(
      cases = c("Small Stones", "Large Stones", "Both"),
      treatmentA = c(paste(cell11count, " ", "(", cell11rate, ")", sep = ""), 
                     paste(cell21count, " ", "(", cell21rate, ")", sep = ""), 
                     paste(cell31count, " ", "(", cell31rate, ")", sep = "")),
      treatmentB = c(paste(cell12count, " ", "(", cell12rate, ")", sep = ""), 
                     paste(cell22count, " ", "(", cell22rate, ")", sep = ""), 
                     paste(cell32count, " ", "(", cell32rate, ")", sep = "")),
      
      stringsAsFactors = FALSE)
    
    rename(df, c("cases"= "Severity",
                 "treatmentA" = "Treatment A", 
                 "treatmentB" = "Treatment B"))
  }) 
  
  # Show the values using an HTML table
  output$table <- renderTable({sliderValues()})
})

# runApp(list(
#   ui=pageWithSidebar(headerPanel("Adding entries to table"),
#                      sidebarPanel(textInput("text1", "Column 1"),
#                                   textInput("text2", "Column 2"),
#                                   actionButton("update", "Update Table")),
#                      mainPanel(tableOutput("table1"))),
#   server=function(input, output, session) {
#     tableStart <- data.frame(Column1 = NA, Column2 = NA)
#     newEntry <- reactive({
#       input$update
#       newLine <- isolate(c(input$text1, input$text2))
#     })
#     output$table1 <- renderTable({rbind(tableStart, newEntry())})
#   }))