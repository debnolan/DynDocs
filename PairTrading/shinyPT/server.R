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
    "ERROR: Optimal K value applicable only if two different stocks are selected."
  } else {
  data=combine2Stocks(a,b)
  bes= getBestK( x= data[,2], y=data[,3])
  print(bes)
  }  
}

funh = function( a ,b) {
  if (identical(a,b)==TRUE) {
    "ERROR: Best profit only application when two different stocks are selected."
  } else {
  data=combine2Stocks(a,b)
  k = getBestK( x= data[,2], y=data[,3])
  tt= getProfit.K(k=k, x=data[,2], y=data[,3])
  print(tt)
  }
}

shinyServer( function( input,output) {
  
  output$plot <- renderPlot({
    funx (att,verizon, FALSE)
  })
  
  output$plot2 <- renderPlot({
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
    
    
    funx (arg.a,arg.b, input$circle)
    
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

