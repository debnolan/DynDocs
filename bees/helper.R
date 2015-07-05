graphGamma = function(alpha=1, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "red", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution", ylim=c(0,7))
}

randPlot = function(alpha=1, beta=1){
  curve(alpha*x, 1, alpha)
}

plotCDF = function(alpha = 1, beta = 1){
  title = paste("CDF of Gamma Distribution with alpha =", alpha, "and beta =", beta)
  plot(ecdf(rgamma(1000, 1,1)), main = title, ylab = "", xlab = "")
}

dgamma(10, shape= 1)



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
