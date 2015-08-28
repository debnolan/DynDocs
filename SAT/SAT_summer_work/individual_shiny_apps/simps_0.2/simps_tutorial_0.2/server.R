library(shiny)
library(plyr)

# Define server logic for slider examples
shinyServer(function(input, output) {

  count_rate_values2 <- reactive({
    success_rate_A_small2= input$success_rate_A_small2
    success_rate_A_large2= input$success_rate_A_large2 
    success_rate_B_small2= input$success_rate_B_small2
    success_rate_B_large2= input$success_rate_B_large2
    
    # Treatment A values
    success_count_A_small2 = 
      round(success_rate_A_small2* input$count_A_small2, digits = 0)
    success_count_A_large2 = 
      round(success_rate_A_large2 * (350 - input$count_A_small2), digits = 0)
    success_count_A_both2 = 
      round((success_count_A_small2 + success_count_A_large2), digits = 0)
    success_rate_A_both2 =
      round((success_count_A_small2 + success_count_A_large2)/ 350 * 100, digits = 0)
    
    # Treatment B values
    success_count_B_small2 = 
      round(success_rate_B_small2 * input$count_B_small2, digits = 0)
    success_count_B_large2 = 
      round(success_rate_B_large2 * (350 - input$count_B_small2), digits = 0)
    success_count_B_both2 = 
      round((success_count_B_small2 + success_count_B_large2), digits = 0)
    success_rate_B_both2 = 
      round((success_count_B_small2 + success_count_B_large2)/ 350 * 100, digits = 0)
    
    df2 <- as.data.frame(cbind(c(success_count_A_small2, success_count_A_large2, success_count_A_both2), 
                      c(success_rate_A_small2, success_rate_A_large2, success_rate_A_both2),
                      c(success_count_B_small2, success_count_B_large2, success_count_B_both2),
                      c(success_rate_B_small2, success_rate_B_large2, success_rate_B_both2)))
    
  })
    
  cell_values2 <- reactive({
    df2 <- count_rate_values2()
#     df2 <- apply(df2, 2, as.character)
    cell11count2 = paste(df2[1,1], "/", as.character(input$count_A_small2), sep = "")
    cell21count2 = paste(df2[2,1], "/", as.character(350 - input$count_A_small2), sep = "")
    cell31count2 = paste(df2[3,1], "/", "350", sep = "")
    cell11rate2 = paste(df2[1,2] * 100, "%", sep = "")
    cell21rate2 = paste(df2[2,2] * 100, "%", sep = "")
    cell31rate2 = paste(df2[3,2], "%", sep = "")
    
    cell12count2 = paste(df2[1,3], "/", as.character(input$count_B_small2), sep = "")
    cell22count2 = paste(df2[2,3], "/", as.character(350 - input$count_B_small2), sep = "")
    cell32count2 = paste(df2[3,3], "/", "350", sep = "")
    cell12rate2 = paste(df2[1,4] * 100, "%", sep = "")
    cell22rate2 = paste(df2[2,4] * 100, "%", sep = "")
    cell32rate2 = paste(df2[3,4], "%", sep = "")
     
    df2 <- data.frame(
      cases2 = c("Small Stones", "Large Stones", "Both"),
      treatmentA2 = c(paste(cell11count2, " ", "(", cell11rate2, ")", sep = ""), 
                     paste(cell21count2, " ", "(", cell21rate2, ")", sep = ""), 
                     paste(cell31count2, " ", "(", cell31rate2, ")", sep = "")),
      treatmentB2 = c(paste(cell12count2, " ", "(", cell12rate2, ")", sep = ""), 
                     paste(cell22count2, " ", "(", cell22rate2, ")", sep = ""), 
                     paste(cell32count2, " ", "(", cell32rate2, ")", sep = "")),
      
      stringsAsFactors = FALSE)
    
    rename(df2, c("cases2"= "Severity",
                 "treatmentA2" = "Treatment A", 
                 "treatmentB2" = "Treatment B"))
  }) 

  
  # Show the values using an HTML table
  output$table <- renderTable({cell_values2()})
  
  output$yesorno <- renderText({
    dff<- count_rate_values2()
    paradox1 = (dff[1,2] < dff[1,4]) & (dff[2,2] < dff[2,4]) & (dff[3,2] > dff[3,4])
    paradox2 = (dff[1,2] > dff[1,4]) & (dff[2,2] > dff[2,4]) & (dff[3,2] < dff[3,4])
    if(isTRUE(paradox1)){
      print("Simpson's Paradox: YES")
    }else{
      if(isTRUE(paradox2)){
        print("Simpson's Paradox: YES")
      }else{
        print("Simpson's Paradox: NO")
      }
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