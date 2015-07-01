library(shiny)
source("helper.R")

funx = function( a ,b , o) {
  data = combine2Stocks( a, b )
  r = data[,2]/data[,3]
  plotRatio(r)
  if(o){
    pos = getPositions( r, 1)
        showPosition(pos,r)
  }
}


funi = function( a ,b) {
  if (identical(a,b)==TRUE) {
   paste( "The optimal K value is:", "ERROR: Optimal K value only applicable when two different stocks are selected.") 
  } else {
  data=combine2Stocks(a,b)
  bes= getBestK( x= data[,2], y=data[,3])
  paste("The optimal K value of this pair of stocks is", sprintf("%1.4f", bes))
  }  
}

funh = function( a ,b) {
  if (identical(a,b)==TRUE) {
  paste(" The best profit is:", "ERROR: Best profit only applicaticable when two different stocks are selected.")
  } else {
  data=combine2Stocks(a,b)
  k = getBestK( x= data[,2], y=data[,3])
  tt= getProfit.K(k=k, x=data[,2], y=data[,3])
  paste( "The best profit possible is", sprintf("%1.4f", tt))
  }
}

shinyServer( function( input,output) {
  
  
  output$plot <- renderPlot({
    funx (att,verizon, FALSE)
  })
  
  data <- reactive({
    
    input$graph
    
    isolate({
      arg.a <- switch(input$a,
                      "ibm" = ibm,
                      "gm" = gm,
                      "inc" = inc,
                      "toyota" = toyota,
                      "hershey" = hershey,
                      "kellog" = kellog,
                      "hyatt" = hyatt,
                      "hilton" = hilton,
                      "united" = united,
                      "southwest" = southwest)
      
      arg.b <- switch(input$b,
                      "ibm" = ibm,
                      "gm" = gm,
                      "inc" = inc,
                      "toyota" = toyota,
                      "hershey" = hershey,
                      "kellog" = kellog,
                      "hyatt" = hyatt,
                      "hilton" = hilton,
                      "united" = united,
                      "southwest" = southwest)
      
      list( arga = input$a, argb = input$b)
      
    })
  })
  
  output$plot2 <- renderPlot({
    if(input$graph) {
      funx( data()$arga, data()$argb, input$circle)
    } else {  frame()
    }   
  })
  
  
  output$best <-renderText({
    arg.a <- switch(input$a,
                    "ibm" = ibm,
                    "gm" = gm,
                    "inc" = inc,
                    "toyota" = toyota,
                    "hershey" = hershey,
                    "kellog" = kellog,
                    "hyatt" = hyatt,
                    "hilton" = hilton,
                    "united" = united,
                    "southwest" = southwest)
    
    
    arg.b <- switch(input$b,
                    "ibm" = ibm,
                    "gm" = gm,
                    "inc" = inc,
                    "toyota" = toyota,
                    "hershey" = hershey,
                    "kellog" = kellog,
                    "hyatt" = hyatt,
                    "hilton" = hilton,
                    "united" = united,
                    "southwest" = southwest)
    funi (arg.a, arg.b)
    
  })
  
  output$besp <-renderText({
        
    arg.a <- switch(input$a,
                    "ibm" = ibm,
                    "gm" = gm,
                    "inc" = inc,
                    "toyota" = toyota,
                    "hershey" = hershey,
                    "kellog" = kellog,
                    "hyatt" = hyatt,
                    "hilton" = hilton,
                    "united" = united,
                    "southwest" = southwest)
    
    
    arg.b <- switch(input$b,
                    "ibm" = ibm,
                    "gm" = gm,
                    "inc" = inc,
                    "toyota" = toyota,
                    "hershey" = hershey,
                    "kellog" = kellog,
                    "hyatt" = hyatt,
                    "hilton" = hilton,
                    "united" = united,
                    "southwest" = southwest)
    funh (arg.a, arg.b)
    
  })
  
})

