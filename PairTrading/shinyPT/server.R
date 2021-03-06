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
 
  data=combine2Stocks(a,b)
  bes= getBestK( x= data[,2], y=data[,3])
  paste("The optimal K  value of this pair of stocks is", sprintf("%.4f", bes))  
}

funh = function( a ,b) {
  data=combine2Stocks(a,b)
  k = getBestK( x= data[,2], y=data[,3])
  tt= getProfit.K(k=k, x=data[,2], y=data[,3])
  paste( "The best profit possible is", sprintf("%1.4f", tt))
  
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
      if(input$a == input$b){same = TRUE}else{same = FALSE}
      
      list( arga = arg.a, argb = arg.b, noShow = same)
      
    })
  })
  
  output$plot2 <- renderPlot({
    if(!data()$noShow) {
      funx( data()$arga, data()$argb, input$circle)
    } else {  return(NULL)
    }   
  })
  
  output$choosetwo = renderText({
    if(data()$noShow){
      "Please select two different stocks"
    }else{
      ""
    }
  })
  
  output$best <-renderText({
   if(data()$noShow){
     paste("Please select two differnet stocks in order to find optimal K.")
   } else {
     funi(data()$arga, data()$argb)
   }
  })
  
  output$besp <-renderText({
    if(data()$noShow){
      paste(" Please select two different stocks in order to find best profit.")
    } else {
      funh(data()$arga, data()$argb)
    }
})

})
