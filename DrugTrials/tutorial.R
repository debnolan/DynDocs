tutPop1 = c(12, 16, 5)
tutPop2 = c(9, 4, 11, 5)
mean(tutPop1)
mean(tutPop2)

tutPop = c(tutPop1, tutPop2)
n = 3
rep = 5
sample(tutPop, n)
perm = replicate(rep, mean(sample(tutPop, n)))
perm

plotPerm = function(pop1, pop2, NRep, comp = mean(pop1)) {
  simPop = replicate(NRep, mean(sample(c(pop1, pop2), length(pop1))))
  plot(table(unique(simPop, simPop)), type = "h",
       xlab = "Average of Sample",
       ylab = "Number of Times")
  abline(v = comp, col = "red")
}

