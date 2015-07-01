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
      t(data.frame(Evidence = "Initial State", 'P.Rain' = input$initrhmm, 'P.Sun' = 1 - input$initrhmm))
    }else{      
      evi = paste("Initial State;", evi, sep = "")
      t(data.frame(Evidence = unlist(strsplit(evi, split = ";")), 'P.Rain' = datahmm()[1, ], 'P.Sun' =  datahmm()[2,]))
    }
  })


  evibio <- reactiveValues(data = vector(mode = "character"))

  observeEvent(input$A, {
    evibio$data <- paste(evibio$data, "A;", sep = "")
  })

  observeEvent(input$C, {
    evibio$data <- paste(evibio$data, "C;", sep = "")
  })
  
  observeEvent(input$G, {
    evibio$data <- paste(evibio$data, "G;", sep = "")
  })

  observeEvent(input$Tb, {
    evibio$data <- paste(evibio$data, "T;", sep = "")
  })
  
  observeEvent(input$clbio, {
    evibio$data <- vector(mode = "character")
  })


  databio <- reactive({
    viter = function(state, emi, ini, observ){
      n = length(observ)
      if(n == 0){
        return(NA)
      }
      obs = vector(length = n)
      obs[observ == "A"] = 1
      obs[observ == "C"] = 2
      obs[observ == "G"] = 3
      obs[observ == "T"] = 4
      Vu = vector(length = n + 1)
      Vd = vector(length = n + 1)
      Vu[1] = log2(ini[1])
      Vd[1] = log2(ini[2])
      for(i in 1:n){
        if(i == 1){
          Vu[i + 1] = log2(emi[1, obs[i]]) + Vu[i]
          Vd[i + 1] = log2(emi[2, obs[i]]) + Vd[i]
        }else{
          Vu[i + 1] = log2(emi[1, obs[i]]) + max(Vu[i] + log2(state[1, 1]), Vd[i] + log2(state[2, 1]))
          Vd[i + 1] = log2(emi[2, obs[i]]) + max(Vu[i] + log2(state[1, 2]), Vd[i] + log2(state[2, 2]))
        }
      }
      path = vector(mode = "character")
      for(i in 1:n){
        if (Vu[i + 1] >= Vd[i + 1]){
          path[i] = "H"
        }else{
          path[i] = "L"
        }
      }
      path = c("NA", path)
      return(matrix(c(Vu, Vd, path), byrow = T, nrow = 3))
    }
    
    viter(state = matrix(c(input$tranhh, 1 - input$tranhh, 1 - input$tranll, input$tranll), nrow = 2, byrow = T), 
            emi = matrix(c(input$emha, 0.5 - input$emha, 0.5 - input$emha, input$emha, 
                           input$emla, 0.5 - input$emla, 0.5 - input$emla, input$emla),
                         nrow = 2, byrow = T), 
            ini = c(input$inith, 1 - input$inith),
            observ = unlist(strsplit(evibio$data, split = ";")))
  })


  output$plotbio <- renderPlot({
    if(is.na(databio()[1])){
      plot( databio(), type = "l", ylim = c( -1, 0), col = "red", xlab = "Evidences", 
            ylab = "Probability", 
            main = "Simulation of Biology Probability", cex.main = 1.6)
      hmmn = length(unlist(strsplit(evibio$data, split = ";")))
      legend(x = hmmn - (hmmn / 10) + 1, y = -0.1,
             legend = c("H", "L"), fill = , c("red", "blue"))
      
    }else{
      plot( databio()[1,], type = "l", ylim = c(min(as.numeric(databio()[1, dim(databio())[2]]), as.numeric(databio()[2,dim(databio())[2]])), 0), col = "red", xlab = "Evidences", 
            ylab = "Probability", 
            main = "Simulation of Biology Probability", cex.main = 1.6)
      hmmn = length(unlist(strsplit(evibio$data, split = ";")))
      legend(x = hmmn - (hmmn / 10) + 1, y = min(as.numeric(databio()[1, dim(databio())[2]]), as.numeric(databio()[2,dim(databio())[2]]))/10,
             legend = c("H", "L"), fill = , c("red", "blue"))
      lines( databio()[2,], col = "blue")
    }  
  })

  output$tablebio <- renderTable({
    evi_2 = evibio$data
    if(length(unlist(strsplit(evi_2, split = ";"))) == 0){
      t(data.frame(Evidence = "Initial State", 'P.H(log2)' = log2(input$initrhmm), 'P.L(log2)' = log2(1 - input$initrhmm)))
    }else{
      evi_2 = paste("Initial State;", evi_2, sep = "")
      t(data.frame(Evidence = unlist(strsplit(evi_2, split = ";")), 
                   'P.H(log2)' = databio()[1, ], 
                   'P.L(log2)' =  databio()[2,],
                    'Path' = databio()[3,]))
    }
})
  
})
