library("shiny")

# law of large numbers w/ binomial

# sliders
# radio buttons
# selection menus
# submit buttons
# check boxes (multiple selection)
# numeric entry
# text box

calPop = c(-11, -5, -5, -4, -3, -3, -3, -2, -1, -1, -1, 
           1, 2, 3, 5, 7, 10, 11, 12, 17, 18)

popHosp = c(11.8, 8.2, 7.1, 13, 10.8, 10.1, 14.6, 14, 
            12.1, 8.3, 3.8, 7.2, 12, 11.2, 10.1, 13.7)

permTest = function(pop, NRep, size, comp) {
  simAvg = replicate(NRep, mean(sample(pop, size)))
  compare = simAvg >= comp
  return(sum(compare)/NRep)
}

permTest(calPop, NRep = 1000, size = 10, comp = 5)
permTest(popHost, NRep = 10000, size = 8, comp = 11.2)

x = 10^seq(0.5, 5, by = 0.1)

pVal = sapply(x, function(n) permTest(calPop,  NRep = n, size = 10, comp = 5))
plot(pVal ~ log10(x), type = "l")
abline(h = pVal[46], lty = 3, col = "grey")

pVal2 = sapply(x, function(n) permTest(popHosp,  NRep = n, size = 8, comp = 11.2))
plot(pVal2 ~ log10(x), type = "l")
abline(h = pVal2[46], lty = 3, col = "grey")
