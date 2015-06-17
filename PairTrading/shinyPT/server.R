library(shiny)
source("helper.R")

funx = function( a ,b ) {
  data = combine2Stocks( a, b )
  r = data[,2]/data[,3]
  plotRatio(r)
}

func = function( a ,b) {
  data = combine2Stocks( a ,b)
  r = data[,2]/data[,3]
  plotRatio(r)
  pos = getPositions( r, 1)
  plotRatio( r, 1 ,col = "lightgray", ylab = "ratio")
  showPosition(pos,r)
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

if (FALSE) { 
shinyServer( function (input, output) {
  
  dot <-reactive({ 
    if (!input$circle) return (funx())
    else func()
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
    
     dot (arg.a, arg.b)
  })
})
}
 