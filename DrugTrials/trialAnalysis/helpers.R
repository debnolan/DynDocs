plotPerm = function(pop1, pop2, NRep, comp) {
  simPop = replicate(NRep, mean(sample(c(pop1, pop2), length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h", xaxt = "n",
       xlab = "Average Change in Blood Pressure in Sample",
       ylab = "Number of Times")
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

calcium = c(7, -4, 18, 17, -3, -5, 1, 10, 11, -2)
placebo = c(-1, 12, -1, -3, 3, -5, 5, 2, -11, -1, -3)