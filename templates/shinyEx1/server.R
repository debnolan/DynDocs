shinyServer(
  function(input, output) {
  
    output$text1 <- renderText({ 
      paste("You have selected", input$var)
    })

    output$text2 <- renderText({ 
      paste("You have chosen a range that goes from",
            input$range[1], "to", input$range[2])
    })
    
  }
)
