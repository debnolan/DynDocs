graphGamma = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  tit = paste("Gamma Distribution, Alpha =", alpha, "and Beta =", beta)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main=tit, ylim=c(0,2), xlim=c(0,7))
  }

graphGamma1 = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
}

graphGamma1half = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1, 1.5 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1.5,1), col=3)
}

graphGamma2 = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1, 1.5, 2 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1.5,1), col=3)
  lines(y, dgamma(y, 2,1), col=4)
}

graphGamma3 = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1, 1.5, 2, 3 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1.5,1), col=3)
  lines(y, dgamma(y, 2,1), col=4)
  lines(y, dgamma(y, 3,1), col=5)
}

graphGamma4 = function(alpha=.5, beta=1) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1, 1.5, 2, 3, 4 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1.5,1), col=3)
  lines(y, dgamma(y, 2,1), col=4)
  lines(y, dgamma(y, 3,1), col=5)
  lines(y, dgamma(y, 4,1), col=6)
}

graphbGamma = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = 1 and Beta = .5", ylim=c(0,2), xlim=c(0,7))
}

graphbGamma1 = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = 1 and Beta = .5, 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
}

graphbGamma1half = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = 1 and Beta = .5, 1, 1.5", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1,1.5), col=3)
}

graphbGamma2 = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = 1 and Beta = .5, 1, 1.5, 2", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1,1.5), col=3)
  lines(y, dgamma(y, 1,2), col=4)
}

graphbGamma3 = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = 1 and Beta = .5, 1, 1.5, 2, 3", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1,1.5), col=3)
  lines(y, dgamma(y, 1,2), col=4)
  lines(y, dgamma(y, 1,3), col=5)
}

graphbGamma4 = function(alpha=1, beta=.5) {
  y <- seq(0, 7, by=.001)
  plot(x=y, y=dgamma(x, alpha, beta), col = "black", type = "l", xlab = "X",
       ylab="Probability Density", main="Gamma Distribution, Alpha = .5, 1, 1.5, 2, 3, 4 and Beta = 1", ylim=c(0,2), xlim=c(0,7))
  lines(y, dgamma(y, 1,1), col=2)
  lines(y, dgamma(y, 1,1.5), col=2)
  lines(y, dgamma(y, 1,2), col=3)
  lines(y, dgamma(y, 1,3), col=4)
  lines(y, dgamma(y, 1,4), col=5)
}


randPlot = function(alpha=1, beta=1){
  curve(alpha*x, 1, alpha)
}

plotDistr = function(n=100){
  title = paste("Histogram of Gamma Distribution with alpha = 1, beta = 1, n = ",n)
  plot(hist(rgamma(n, 1,1)), main = title, ylab = "", xlab = "")
}


plotCDF = function(n=100){
  title = paste("CDF of Gamma Distribution with alpha = 1 and beta = 1 and n= ", n)
  plot(ecdf(rgamma(n, 1,1)), main = title, ylab = "", xlab = "")
}




plotDataGammaQ = function(probs = seq(0.05, 0.95, by = 0.05), bee){
  distance = c(75  ,65,  40 , 60 , 50 , 55, 130,  45, 105, 140, 120  ,80  ,75,  60,  60,  35,  70,  35,  55,  90, 
               260, 175,  70 , 85 ,140, 120, 105,  90,  80,  65,  70,  50, 115, 100,  95,  85,  30,  10,  60,  35,
               40,  55, 250, 300,  90)
  
  
  ttime= c(4.0,  3.0,  1.0,  5.0,  1.0,  1.0,  1.0,  1.0,  5.0,  4.0,  5.0, 5.0,  5.0, 12.0, 11.5,  2.0,
           2.0,  2.0,  2.0,  1.0 ,10.0,  1.0,  7.0,  7.0,  2.0,  1.0,  2.0,  3.0,  1.0,  1.0,  1.0,  1.0,
           4.0,  4.0,  3.0,  3.0,  2.0,  2.0,  3.0,  3.0,  7.0,  3.0,  4.0, 5.0,  2.0)
  
  
  
  Transit.Time= c(5.3 , 4.6 , 2.5,  8.3,  2.0,  1.8,  0.8,  2.2,  4.8,  2.9,  4.2, 6.3,  6.7, 20.0, 19.2,  5.7,
                2.9,  5.7,  3.6,  1.1,  3.8,  0.6, 10.0,  8.2,  1.4,  0.8,  1.9,  3.3,  1.3,  1.5,  1.4,  2.0,
                3.5,  4.0,  3.2,  3.5,  6.7, 20.0,  5.0,  8.6, 17.5,  5.5,  1.6,  1.7  ,2.2)
  
  bee = data.frame(distance, ttime, Transit.Time)
  dataQ = quantile(bee$Transit.Time, probs = probs)
gammaQ  = qgamma(probs, 1.61, 1/2.37)
plot(dataQ~gammaQ, col = "blue", pch = 19, xlim = c(-3,20), ylim = c(0,20), xlab= 'Theoretical Observations', ylab = 'Data Observations')

}

plotDataNormQ = function(probs = seq(0.05, 0.95, by = 0.05), bee){
  distance = c(75  ,65,  40 , 60 , 50 , 55, 130,  45, 105, 140, 120  ,80  ,75,  60,  60,  35,  70,  35,  55,  90, 
               260, 175,  70 , 85 ,140, 120, 105,  90,  80,  65,  70,  50, 115, 100,  95,  85,  30,  10,  60,  35,
               40,  55, 250, 300,  90)
  
  
  ttime= c(4.0,  3.0,  1.0,  5.0,  1.0,  1.0,  1.0,  1.0,  5.0,  4.0,  5.0, 5.0,  5.0, 12.0, 11.5,  2.0,
           2.0,  2.0,  2.0,  1.0 ,10.0,  1.0,  7.0,  7.0,  2.0,  1.0,  2.0,  3.0,  1.0,  1.0,  1.0,  1.0,
           4.0,  4.0,  3.0,  3.0,  2.0,  2.0,  3.0,  3.0,  7.0,  3.0,  4.0, 5.0,  2.0)
  
  
  
  Transit.Time= c(5.3 , 4.6 , 2.5,  8.3,  2.0,  1.8,  0.8,  2.2,  4.8,  2.9,  4.2, 6.3,  6.7, 20.0, 19.2,  5.7,
                  2.9,  5.7,  3.6,  1.1,  3.8,  0.6, 10.0,  8.2,  1.4,  0.8,  1.9,  3.3,  1.3,  1.5,  1.4,  2.0,
                  3.5,  4.0,  3.2,  3.5,  6.7, 20.0,  5.0,  8.6, 17.5,  5.5,  1.6,  1.7  ,2.2)
  
  bee = data.frame(distance, ttime, Transit.Time)
  dataQ = quantile(bee$Transit.Time, probs = probs)
  normQ = qnorm(probs, mean = mean(bee$Transit.Time), sd = sd(bee$Transit.Time))
plot(dataQ~normQ, col = "red", pch = 19, xlim = c(-3,20), ylim = c(0,20), main = "Normal vs Gamma QQ Plotted against Data", xlab= 'Theoretical Observations', ylab = 'Data Observations')
par(new=TRUE)
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

x <- seq(0, 7, by=.001)
plot(x, dgamma(x, 1, 0.5), type="l",
     ylim=c(0,2), ylab="Density",
     main="Gamma Densities: shape=.5, 1, 1.5, 2, 3, 4;
     scale = 1")
lines(x, dgamma(x, 1,1), col=2)
lines(x, dgamma(x, 1, 1.5), col=3)
lines(x, dgamma(x, 1, 2), col=4)
lines(x, dgamma(x, 1, 3), col=5)
lines(x, dgamma(x, 1, 4), col=6)




