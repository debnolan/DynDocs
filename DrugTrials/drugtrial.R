library("shiny")

setwd("C:/Users/Joseph/DynDocs/templates")
runApp("shinyEx1")
runApp("shinyEx2")
runApp("census-app")

# law of large numbers w/ binomial

# sliders
# radio buttons
# selection menus
# submit buttons
# check boxes (multiple selection)
# numeric entry
# text box

calcium = c(7, -4, 18, 17, -3, -5, 1, 10, 11, -2)
placebo = c(-1, 12, -1, -3, 3, -5, 5, 2, -11, -1, -3)

popHosp1 = c(11.8, 8.2, 7.1, 13, 10.8, 10.1, 14.6, 14) 

popHosp2 = c(12.1, 8.3, 3.8, 7.2, 12, 11.2, 10.1, 13.7)

permTest = function(pop1, pop2, NRep, comp = mean(pop1)) {
  #pop1 and pop2 are randomly sampled in different conditions
  #generates NRep random data sets with size equal to pop1
  #compares mean values of sim data with mean of pop1 (or other threshold)
  #returns a p-values
  
  pop = c(pop1, pop2)
  simAvg = replicate(NRep, mean(sample(pop, length(pop1))))
  compare = simAvg >= comp
  return(sum(compare)/NRep)
}

permTest(calcium, placebo, NRep = 10000, comp = 5)

permTest(popHosp1, popHosp2, NRep = 10000, comp = 11.2)

pValPlot = function(pop1, pop2, comp, x = 10^seq(0.5, 5, by = 0.1)) {
  #x is a distribtuion of logarthmically increasing values
  #creates a plot to show amount of repetitions with decreasing pValue error
  pVal = sapply(x, function(n) permTest(pop1, pop2,  NRep = n, comp = comp))
  plot(pVal ~ log10(x), type = "l")
  pHat = pVal[length(pVal)]
  abline(h = pHat, lty = 3, col = "grey")
  text(4, 0.025, labels = as.character(pHat))
}

pValPlot(calcium, placebo, comp = 5)
pValPlot(popHosp1, popHosp2, comp = 11.2)

fisherTest = function(pop1, pop2, NRep, comp, thresh = NA, bin = FALSE) {
  #fisher's exact test on binary variables (e.g. NO = 0, YES = 1)
  #if two populates are quantitative, dichotomize based on a threshold value
  #generates NRep random data sets and counts the number of "YES" values
  #samples number of "YES" values from original dichotomization 
  #compares number of "YES" values in simulated data with that of pop1
  #returns a p-value
  
  if(!bin) {
    pop = c(pop1, pop2)
    dich = as.numeric(pop >= thresh)
    simAvg = replicate(NRep, sum(sample(dich, comp)))
    compare = simAvg >= length(pop1)
  } else {
    pop = c(pop1, pop2)
    simAvg = replicate(NRep, sum(sample(pop, comp)))
    compare = simAvg >= sum(pop1)
  }
  return(sum(compare)/NRep)
}

medianTest = function(pop1, pop2, NRep) {
  #pop1 and pop2 are quantitative, function dichotomizes them
  #same as fisher's exact test, but with threshold as median of entire population
  #returns a p-value
  
  pop = c(pop1, pop2)
  dich = as.numeric(pop >= median(pop))
  simAvg = replicate(NRep, sum(sample(dich, sum(dich))))
  compare = simAvg >= length(pop1)
  return(sum(compare)/NRep)
}

fisherTest(calcium, placebo, NRep = 10000, comp = 10, thresh = 0)
medianTest(calcium, placebo, NRep = 10000)

replicate(20, 
          fisherTest(calcium, placebo, NRep = 10000, comp = 10, thresh = 0))

fisherTest(pop2 = rep(0, 26), pop1 = rep(1, 35), 
           NRep = 10000, comp = 32, thresh = 35, bin = TRUE)

medianTest(popHosp1, popHosp2, NRep = 100000)

wilTest = function(pop1, pop2, NRep, comp) {
  pop = rank(c(pop1, pop2))
  simAvg = replicate(NRep, mean(sample(pop, length(pop1))))
  compare = simAvg >= pop[which(c(pop1, pop2) == comp)][1]
  return(sum(compare)/NRep)
}

wilTest(calcium, placebo, NRep = 10000, comp = 5)

popJob1 = c(25, 33, 35, 38, 48, 55, 56)
popJob2 = c(55, 55, 64)

wilTest(popJob1, popJob2, NRep = 10000, comp = 55)

http://www.math.uah.edu/stat/data/Polio.html

Vac = c(200745, 33, 24, 25)
Plac = c(201229, 115,  27, 20)
NI = c(338778, 121, 36, 25)

DichPop = function(total, ones) {
  zero = rep(0, total - ones)
  one = rep(1, ones)
  return(c(zero, one))
}


fisherTest(pop1 = DichPop(Plac[1], Plac[2] + Plac[3]), 
           pop2 = DichPop(Vac[1], Vac[2] + Vac[3]), NRep = 100000, 
           comp = 200000, bin = TRUE)

fisherTest(pop1 = DichPop(NI[1], NI[2] + NI[3]), 
           pop2 = DichPop(Vac[1], Vac[2] + Vac[3]), NRep = 1000, 
           comp = 200000, bin = TRUE)

fisherTest(pop1 = DichPop(Plac[1], Plac[2]), 
           pop2 = DichPop(Vac[1], Vac[2]), NRep = 1000, 
           comp = 200000, bin = TRUE)

fisherTest(pop1 = DichPop(Plac[1], Plac[3]), 
           pop2 = DichPop(Vac[1], Vac[3]), NRep = 1000, 
           comp = 200000, bin = TRUE)
