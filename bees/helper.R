graphGamma = function(alpha=1, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, rate = beta), col = "red", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution", ylim=c(0,7))
}

plotCDF = function(n=1000, alpha = 1, beta = 1){
  title = paste("CDF of Gamma Distribution with alpha =", alpha, "and beta =", beta)
  plot(ecdf(rgamma(n, alpha, rate = beta)), main = title, ylab = "", xlab = "")
}
#The plotCDF function takes in a vector 

#dgamma(10, shape= 1)


plotDataGammaQ = function(probs = seq(0.05, 0.95, by = 0.05), bee)
  dataQ = quantile(bee$Transit.Time, probs = probs)
  gammaQ  = qgamma(probs, 1.61, 1/2.37)
  plot(dataQ~gammaQ, xlim = c(-3,20), ylim = c(0,20), xlab= 'Theoretical Observations', ylab = 'Data Observations')

plotDataNormQ = function(probs = seq(0.05, 0.95, by = 0.05), bee)
  normQ = qnorm(probs, mean = mean(bee$Transit.Time), sd = sd(bee$Transit.Time))
  plot(dataQ~normQ, pch=19, xlab= 'Theoretical Observations', ylab = 'Data Observations')

x <- seq(0, 7, by=.001)
plot(x, dgamma(x, 1/2, 1), type="l",
     ylim=c(0,2), ylab="Density",
     main="Gamma Densities: shape=.5, 1, 1.5, 2, 3, 4;
     scale = 1")
lines(x, dgamma(x, 1,1), col=2)
lines(x, dgamma(x, 1.5, 1), col=3)
lines(x, dgamma(x, 2, 1), col=4)
lines(x, dgamma(x, 3, 1), col=5)
lines(x, dgamma(x, 4, 1), col=6)
