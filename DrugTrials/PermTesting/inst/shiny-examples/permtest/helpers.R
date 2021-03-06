#input two drug trial populations (placebo and trial drug)
#run the permutations test for mean with sample repetitions = NRep
#compute a p-value
permTest = function(pop1, pop2, NRep, comp = mean(pop1)) {
  pop = c(pop1, pop2)
  simAvg = replicate(NRep, mean(sample(pop, length(pop1))))
  compare = simAvg >= comp
  return(sum(compare)/NRep)
}

#input two drug trial populations (placebo and trial drug)
#run the permutations test for mean with sample reps = NRep
#plot the distribution of unique means across the sampled pops
#compare the original statistic (mean) via a red vertical line
#compute the p-value and provide it as the title
plotPerm = function(pop1, pop2, NRep) {
  simPop = replicate(NRep, mean(sample(c(pop1, pop2), length(pop1))))
  plot(table(unique(round(simPop, digits = 3), round(simPop, digits = 3))), 
       type = "h", xaxt = "n",
       xlab = "Average of Sample",
       ylab = "Number of Times")
  axis(1, round(unique(simPop), digits = 3))
  abline(v = mean(pop1), col = "red")
  compare = simPop >= mean(pop1)
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", sprintf("%1.05s", as.character(pVal))))
}

#pass a vector of logarithmically increasing NReps to permTest
#plot the p-values against the log10(number of repetitions = NRep)
#provide a horizontal line for most accurate p-value to show convergence
#parameter big sets the x-axis max value based on the data input
pValPlot = function(pop1, pop2, min, max, freq = .1, big = "NULL") {
  x = 10^seq(from = min, to = max, by = freq)
  pVal = sapply(x, function(n) permTest(pop1, pop2,  NRep = n))
  if(big == "HIV") {
    k = 1
  }
  if(big == "Calcium") {
    k = 0.4
  }
  if(big == "Alcohol") {
    k = 0.2
  }
  plot(x = log10(x), y = seq(0, k, length = length(x)),  
       type = "n", yaxt = "n",
       xlab = "Log of Range of Repetitions", ylab = "pHat")
  lines(x = log10(x), y = pVal)
  axis(2, seq(0, k, by = 0.05))
  pHat = pVal[length(pVal)]
  abline(h = pHat, lty = 3, col = "grey")
  title(main = paste("pValue =", sprintf("%1.05s", as.character(pHat))))
}

#dichotomize two drug trial pops (placebo and trial drug) by a threshold
#run the permutations test for the sum of dichotomized sample reps = NRep
#plot the distribution of unique sums across the sampled dichotomized popss
#compare the original statistic (dichotomized sum) via a red vertical line
#compute the p-value and provide it as the title
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
  title(main = paste("pValue =", sprintf("%1.05s", as.character(pVal))))
}

#vizualize the dichotomization process for a specific threshold with dataframe
#changes names depending on the data name of pop1 and pop2
dichData = function(pop1, pop2, threshold, names = "NULL") {
  data = data.frame(pop1 = pop1, 
                    dich1 = as.numeric(pop1 >= threshold),
                    placebo = pop2, dich2 = as.numeric(pop2 >= threshold))
  if(names == "Calcium") {
    names(data) = c("Calcium", "Calcium Dichotomization", 
                    "Placebo", "Placebo Dichotomization")
  }
  if(names == "Alcohol") {
    names(data) = c("Alcohol", "Alcohol Dichotomization", 
                    "Placebo", "Placebo Dichotomization")
  }
  if(names == "HIV") {
    names(data) = c("New Drug", "New Drug Dichotomization",
                    "Old Drug", "Old Drug Dichotomization")
  }
  return(data)
}

#rank two drug trial populations (placebo and trial drug)
#run the permutations test for sum of ranked pop sample repetitions = NRep
#plot the distribution of unique means across the sampled populations
#compare the original statistic (rank-sum) via a red vertical line
#compute the p-value and provide it as the title
wilPlot = function(pop1, pop2, NRep) {
  pop = rank(c(pop1, pop2))
  simPop = replicate(NRep, sum(sample(pop, length(pop1))))
  plot(table(ceiling(unique(simPop, simPop))), type = "h",
       xlab = "Ranked Sum of Drug Data",
       ylab = "Number of Times")
  abline(v = sum(pop[1:length(pop1)]), col = "red")
  compare = simPop >= sum(pop[1:length(pop1)]) 
  pVal = sum(compare)/NRep
  title(main = paste("pValue =", sprintf("%1.05s", as.character(pVal))))
}

#vizualize the ranking process for a specific threshold with dataframe
#changes names depending on the data name of pop1 and pop2
rankData = function(pop1, pop2, names) {
  ranks = rank(c(pop1, pop2), na.last = "keep")
  data = data.frame(pop1, ranks[1:length(pop1)],
                    pop2, ranks[(length(pop1)+1):length(ranks)])
if(names == "Calcium") {
  names(data) = c("Calcium", "Ranked Calcium", 
                  "Placebo", "Ranked Placebo")
}
if(names == "Alcohol") {
  names(data) = c("Alcohol", "Ranked Alcohol", 
                  "Placebo", "Ranked Placebo")
}
if(names == "HIV") {
  names(data) = c("New Drug", "Ranked New Drug",
                  "Old Drug", "Ranked Old Drug")
}
return(data)
}