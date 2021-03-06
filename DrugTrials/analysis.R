#trial of Permutations Test analysis

setwd("C:/Users/Joseph/DynDocs/DrugTrials")
runApp("trialAnalysis")

calcium = c(7, -4, 18, 17, -3, -5, 1, 10, 11, -2)
placebo = c(-1, 12, -1, -3, 3, -5, 5, 2, -11, -1, -3)

# code for helper functions

permTest = function(pop1, pop2, NRep, comp = mean(pop1)) {
  pop = c(pop1, pop2)
  simAvg = replicate(NRep, mean(sample(pop, length(pop1))))
  compare = simAvg >= comp
  return(sum(compare)/NRep)
}

fisherTest = function(pop1, pop2, NRep, thresh) {
    pop = c(pop1, pop2)
    dich = as.numeric(pop >= thresh)
    simAvg = replicate(NRep, sum(sample(dich, length(pop1))))
    compare = simAvg >= length(pop1)
  return(sum(compare)/NRep)
}

dichPlot = function(pop1, pop2, NRep, thresh) {
  pop = c(pop1, pop2)
  dich = as.numeric(pop >= thresh)
  simPop = replicate(NRep, sum(sample(dich, length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h",
       xlab = "Number of Samples Greater than Threshold",
       ylab = "Number of Times")
}


wilTest = function(pop1, pop2, NRep, comp) {
  pop = rank(c(pop1, pop2))
  simAvg = replicate(NRep, mean(sample(pop, length(pop1))))
  compare = simAvg >= pop[which(c(pop1, pop2) == comp)][1] 
  return(sum(compare)/NRep)
}

wilPlot = function(pop1, pop2, NRep, comp) {
  pop = rank(c(pop1, pop2))
  simPop = replicate(NRep, mean(sample(pop, length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h",
       xlab = "Ranked Mean of Blood Pressure Change",
       ylab = "Number of Times")
}

plotPerm = function(pop1, pop2, NRep) {
  simPop = replicate(NRep, mean(sample(c(pop1, pop2), length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h", xaxt = "n",
       xlab = "Average Change in Blood Pressure in Sample",
       ylab = "Number of Times")
  axis(1, floor(unique(simPop)))
}

pValPlot = function(pop1, pop2, comp, range = c(0.5, 5), freq = .25) {
  x = 10^seq(range[1], range[2], by = freq)
  pVal = sapply(x, function(n) permTest(pop1, pop2,  NRep = n, comp = comp))
  plot(pVal ~ log10(x), type = "l")
  pHat = pVal[length(pVal)]
  abline(h = pHat, lty = 3, col = "grey")
  text(range[2] - 0.5, max(pVal) - 0.05, labels = paste("pHat =", as.character(pHat)))
}


fisherTest = function(pop1, pop2, NRep, comp = pop1[1]){
  pop = c(rep(1, pop1[2] + pop2[2]), 
          rep(0, pop1[1] + pop2[1] - pop1[2] - pop2[2]))
  simPop = replicate(NRep, sum(sample(pop, comp)))
  plot(table(unique(simPop, simPop)), type = "h",
       xlab = "Number of 'Yes' Sample Values",
       ylab = "Number of Times")
  compare = simPop >= pop1[2]
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", as.character(pVal)))
}
