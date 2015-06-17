plotPerm = function(pop1, pop2, NRep, comp) {
  simPop = replicate(NRep, mean(sample(c(pop1, pop2), length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h", xaxt = "n",
       xlab = "Average Change in Blood Pressure in Sample",
       ylab = "Number of Times")
  abline(v = 5, col = "red")
  compare = simPop >= comp
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", as.character(pVal)))
  axis(1, floor(unique(simPop)))
}

pValPlot = function(pop1, pop2, comp = 5, min, max, freq = .1) {
  x = 10^seq(from = min, to = max, by = freq)
  pVal = sapply(x, function(n) permTest(pop1, pop2,  NRep = n, comp = comp))
  plot(pVal ~ log10(x), type = "l", 
       xlab = "Log of Range of Repetitions", ylab = "pHat")
  pHat = pVal[length(pVal)]
  abline(h = pHat, lty = 3, col = "grey")
  title(main = paste("pValue =", as.character(pHat)))
}

dichPlot = function(pop1, pop2, NRep, thresh) {
  pop = c(pop1, pop2)
  dich = as.numeric(pop >= thresh)
  simPop = replicate(NRep, sum(sample(dich, length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h",
       xlab = "Number of Sample Values Greater than Threshold",
       ylab = "Number of Times")
  abline(v = sum(dich[1:length(pop1)]) - 0.5, col = "red")
  compare = simPop >= sum(dich[1:length(pop1)])
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", as.character(pVal)))
}

dichData = function(pop1, pop2, threshold) {
  return(data.frame(calcium = c(pop1, NA), 
                    dich1 = c(as.numeric(pop1 >= threshold), NA),
                    placebo = pop2, dich2 = as.numeric(pop2 >= threshold)))
}

wilPlot = function(pop1, pop2, NRep) {
  pop = rank(c(pop1, pop2))
  simPop = replicate(NRep, sum(sample(pop, length(pop1))))
  plot(table(ceiling(unique(simPop, simPop))), type = "h",
       xlab = "Ranked Sum of Blood Pressure Change",
       ylab = "Number of Times")
  abline(v = 124.5, col = "red")
  compare = simPop >= sum(pop[1:length(pop1)]) 
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", as.character(pVal)))
}

rankData = function(pop1, pop2) {
  return(data.frame(calcium = c(pop1, NA), 
            rankedCalcium = c(rank(c(pop1, pop2))[1:length(pop1)], NA),
            placebo = pop2, 
            rankedPlacebo = rank(c(pop1, pop2))[length(pop1)+1:length(pop2)])
              )
}

drugData = function(char) {
  if(char == "Calcium") {
    return(data.frame(calcium, placebo1))
  }
  if(char == "Alcohol") {
    return(Alcohol = data.frame(alcohol, placebo2))
  }
}

calcium = c(7, -4, 18, 17, -3, -5, 1, 10, 11, -2, NA)
placebo1 = c(-1, 12, -1, -3, 3, -5, 5, 2, -11, -1, -3)

placebo2 = c(0.9, 0.37, 1.63, 0.83, 0.95, 0.78, 0.86, 0.61, 0.38, 1.97)
alcohol = c(1.46, 1.45, 1.76, 1.44, 1.11, 3.07, 0.98, 1.27, 2.56, 1.32)

Calcium = data.frame(calcium, placebo1)
Alcohol = data.frame(alcohol, placebo2)