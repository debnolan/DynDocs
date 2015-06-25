library(HMM)
library(expm)
library(shiny)



shinyServer(function(input, output) {
  
  data0 <- reactive({
    rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
    (rsmat0 %^% 100)[1, ]
  })
  
  data1 <- reactive({
    statdist = function( ini, k){ 
      rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
      sapply(0:k, function(i) ini %*% (rsmat0 %^% i) )
    }
    statdist(ini = c(input$initr, 1 - input$initr), k = input$n)[1, ]
  })
  
  data2 <- reactive({
    statdist = function( ini, k){ 
      rsmat0 = matrix(c(input$tranrr, 1 - input$tranrr, 1 - input$transs, input$transs),
                      nrow = 2, byrow = T)
      sapply(0:k, function(i) ini %*% (rsmat0 %^% i) )
    }
    statdist(ini = c(input$initr, 1 - input$initr), k = input$n)[2, ]
  })
  
  
  
  output$plot <- renderPlot({
    plot(data1() , type = "l", ylim = c(0,1), col = "blue", xlab = "Steps", 
         ylab = "Probability", 
         main = "Simulation of Weather Probability", cex.main = 1.6)
    lines(data2() , col = "red")
    abline(h = data0()[1], lwd = 2, col = "blue")
    abline(h = data0()[2], lwd = 2, col = "red")
    legend(x = input$n - (input$n / 10) + 1, y = 0.2, legend = c("sun", "rain"), fill = , c("red", "blue"))
    text(x = 1, y = data1()[1], labels = as.character(input$initr))
    text(x = 1, y = data2()[1], labels = as.character(1 - input$initr))
  })
  
  
  evi <- reactiveValues(data = vector(mode = "character"))
  
  observeEvent(input$um, {
    evi$data <- paste(evi$data, "um;", sep = "")
  })
  
  observeEvent(input$num, {
    evi$data <- paste(evi$data, "num;", sep = "")
  })
  
  observeEvent(input$cl, {
    evi$data <- vector(mode = "character")
  })
  
  datahmm <- reactive({
    beliefN = function(state, symb, ini, observ){
      n = length(observ)
      obs = vector(length = n)
      obs[observ == "um"] = 1
      obs[observ == "num"] = 2
      Ba = ini
      Bu = vector(length = n + 1)
      Bd = vector(length = n + 1)
      Bu[1] = Ba[1]
      Bd[1] = Ba[2]
      for(i in 1:n){
        Bpa = c(sum(Ba * state[, 1]), sum(Ba * state[, 2]))
        Ba =  (Bpa * symb[, obs[i]]) / sum(Bpa * symb[, obs[i]])
        Bu[ i + 1] = Ba[1]
        Bd[ i + 1] = Ba[2]
      }
      return(matrix(c(Bu, Bd), byrow = T, nrow = 2))
    }
    beliefN(state = matrix(c(input$tranrrhmm, 1 - input$tranrrhmm, 1 - input$transshmm, input$transshmm), nrow = 2, byrow = T), 
            symb = matrix(c(input$emru, 1 - input$emru, input$emsu, 1 - input$emsu), nrow = 2, byrow = T), 
            ini = c(input$initrhmm, 1 - input$initrhmm),
            observ = unlist(strsplit(evi$data, split = ";")))
  })
  
  
  
  output$plothmm <- renderPlot({
    plot( datahmm()[1,], type = "l", ylim = c(0,1), col = "blue", xlab = "Evidences", 
         ylab = "Probability", 
         main = "Simulation of Weather Probability", cex.main = 1.6)
    hmmn = length(unlist(strsplit(evi$data, split = ";")))
    legend(x = hmmn - (hmmn / 10) + 1, y = 0.2, legend = c("sun", "rain"), fill = , c("red", "blue"))
    lines( datahmm()[2,], col = "red")
    
  })
  
  # Generate a summary of the data
#  output$summary <- renderPrint({
#    summary(data())
#  })
  
  output$table <- renderTable({
    evi = gsub("num", "-u", evi$data)
    evi = gsub("um", "+u", evi)
    if(length(unlist(strsplit(evi, split = ";"))) == 0){
      data.frame(x = unlist(strsplit(evi, split = ";")))
    }else{      
      t(data.frame(Evidence = unlist(strsplit(evi, split = ";"))))
    }
  })
  
})
