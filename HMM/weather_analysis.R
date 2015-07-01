library(HMM)
library(expm)

#1#

rsmat0 = matrix(c(0.7, 0.3, 0.1, 0.9), nrow = 2, byrow = T)

statdist = function(ini, k){ 
  sapply(0:k, function(i) ini %*% (rsmat0 %^% i) )
}


plot(statdist(c(1,0), 10)[1,] , type = "l", ylim = c(0,1), col = "blue")
lines(statdist(c(1,0), 10)[2,], col = "red")

abline(h = 0.25, lwd = 2, col = "blue")
abline(h = 0.75, lwd = 2, col = "red")

lines(statdist(c(0.5,0.5), 10)[1,], col = "blue", lty = 2)
lines(statdist(c(0.5,0.5), 10)[2,], col = "red", lty = 2)

legend(x = 8, y = 0.2, legend = c("sun", "rain"), fill = , c("red", "blue"))
text(x = 1, y = statdist(c(1,0), 10)[1, 1], labels = "1.0")
text(x = 1, y = statdist(c(1,0), 10)[2, 1], labels = "0")
text(x = 1, y = statdist(c(0.5, 0.5), 10)[1, 1], labels = "0.5")
text(x = 1, y = statdist(c(0.5, 0.5), 10)[2, 1], labels = "0.5")

#2#

rsmat = matrix(c(0.7, 0.3, 0.3, 0.7), 2)

wumat = matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = T)

B0 = c(0.5, 0.5)

Bp1 = c(sum(B0 * rsmat[, 1]), sum(B0 * rsmat[, 2]))

B1 =  (Bp1 * wumat[, 1]) / sum(Bp1 * wumat[, 1])  

Bp2 = c(sum(B1 * rsmat[, 1]), sum(B1 * rsmat[, 2]))

B2 =  (Bp2 * wumat[, 1]) / sum(Bp2 * wumat[, 1])  

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

B = beliefN(rsmat, wumat, B0, c("um", "um"))


weahmm = initHMM(c("rain", "sun"), c("um", "num"), c(0.5 , 0.5), rsmat, wumat)

observ = c("um", "um")

forw = exp(forward(weahmm, observ))
     
B_for = matrix(c(forw[,1]/ sum(forw[,1]), forw[,2]/ sum(forw[,2])), nrow = 2)


