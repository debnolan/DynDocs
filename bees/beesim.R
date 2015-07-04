bee = read.table("beedata.csv", sep=",", header=TRUE)
hist(bee$Transit.Time, breaks=seq(-01, 21, by=2), prob = TRUE, main = "Histogram of Estimated Transit Times",
     xlab = "Estimated transit time (mo per 100 km)", ylim = c(0,0.2))

# lines(density(bee$Transit.Time), lwd = 2, col = "blue")

qqnorm(y=bee$Transit.Time)
qqline(y = bee$Transit.Time)
#we see that if we would just use a normal distribution, the normal distribution 
#would understate the extreme positive values of x

#install.packages("MASS")
library(MASS)

normest = fitdistr(bee$Transit.Time, "normal")
curve(dnorm(x, normest$estimate[1], normest$estimate[2]), add=TRUE, col='red')

gammaest = fitdistr(bee$Transit.Time, densfun="gamma")
curve(dgamma(x, gammaest$estimate[1], gammaest$estimate[2]), add=TRUE, col='blue')
x=seq(0,20, by=0.1)
lines(x, dgamma(x, 1.61,1/2.37), col=2)

legend("topright", cex=0.75, pch=16, 
       col=c("red", "blue"), legend=c("Normal Dist", "Gamma Dist"))

if (FALSE) {
  probs = seq(0.05, 0.95, by = 0.05)
  dataQ = quantile(bee$Transit.Time, probs = probs)
  gammaQ  = qgamma(probs, 1.61, 1/2.37)
  normQ = qnorm(probs, mean = mean(bee$Transit.Time), sd = sd(bee$Transit.Time))
  
  plot(dataQ~gammaQ, xlim = c(-3,20), ylim = c(0,20))
  points(dataQ, x = normQ, pch = 19)
}
#curve(dgamma(x= BeeTable$Transit.Time, shape=2, scale=1.5), from= 0, to = 20, main = "Gamma Distribution")
# dgamma(x= bee$Transit.Time, shape=1, scale=1.5)
#make a slider thingy to adjust the shape and the scale


