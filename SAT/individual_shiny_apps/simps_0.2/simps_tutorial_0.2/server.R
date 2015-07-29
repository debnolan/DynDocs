library(shiny)
library(plyr)

# Define server logic for slider examples
shinyServer(function(input, output) {

  count_rate_values <- reactive({
    success_rate_A_small = 0.93
    success_rate_A_large = 0.73
    success_rate_B_small = 0.87
    success_rate_B_large = 0.69
    
    # Treatment A values
    success_count_A_small = 
      round(success_rate_A_small * input$count_A_small, digits = 0)
    success_count_A_large = 
      round(success_rate_A_large * (350 - input$count_A_small), digits = 0)
    success_count_A_both = 
      round((success_count_A_small + success_count_A_large), digits = 0)
    success_rate_A_both =
      round((success_count_A_small + success_count_A_large)/ 350 * 100, digits = 0)
    
    # Treatment B values
    success_count_B_small = 
      round(success_rate_B_small * input$count_B_small, digits = 0)
    success_count_B_large = 
      round(success_rate_B_large * (350 - input$count_B_small), digits = 0)
    success_count_B_both = 
      round((success_count_B_small + success_count_B_large), digits = 0)
    success_rate_B_both = 
      round((success_count_B_small + success_count_B_large)/ 350 *100, digits = 0)
    
    df<- data.frame(countA = c(success_count_A_small, success_count_A_large, success_count_A_both), 
               rateA = c(success_rate_A_small, success_rate_A_large, success_rate_A_both),
               countB = c(success_count_B_small, success_count_B_large, success_count_B_both),
               rateB = c(success_rate_B_small, success_rate_B_large, success_rate_B_both)
    )
  })
    
  cell_values <- reactive({
    df <- count_rate_values()
    df <- apply(df, 2, as.character)
    cell11count = paste(df[1,1], "/", as.character(input$count_A_small), sep = "")
    cell21count = paste(df[2,1], "/", as.character(350 - input$count_A_small), sep = "")
    cell31count = paste(df[3,1], "/", "350", sep = "")
    cell11rate = paste(as.numeric(df[1,2]) * 100, "%", sep = "")
    cell21rate = paste(as.numeric(df[2,2]) * 100, "%", sep = "")
    cell31rate = paste(df[3,2], "%", sep = "")
    
    cell12count = paste(df[1,3], "/", as.character(input$count_B_small), sep = "")
    cell22count = paste(df[2,3], "/", as.character(350 - input$count_B_small), sep = "")
    cell32count = paste(df[3,3], "/", "350", sep = "")
    cell12rate = paste(as.numeric(df[1,4]) * 100, "%", sep = "")
    cell22rate = paste(as.numeric(df[2,4]) * 100, "%", sep = "")
    cell32rate = paste(df[3,4], "%", sep = "")
     
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
  output$table <- renderTable({cell_values()})
  
  output$yesorno <- renderText({
    df<- count_rate_values()
    if(df[3,2] < df[3,4]){
      print("Simpson's Paradox: YES")
    }else{
      print("Simpson's Paradox: NO")
    }
  })
  
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