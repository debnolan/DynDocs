library(shiny)
source("helper.R")

funx = function( a ,b ) {
  data = combine2Stocks( a, b )
  r = data[,2]/data[,3]
  plotRatio(r)
}


shinyServer( function( input,output) {

  
  output$plot <- renderPlot({
    funx (att,verizon)
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
    funx (arg.a,arg.b)
  })
})