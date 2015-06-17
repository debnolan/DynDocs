library(extRemes)
setwd("~/GitHub/DynDocs/rainfall")
load("data/FrontRange.rda")


fit1 = fevd(precip, rainfall[[1]], threshold = 30,
            type="GP", units = "mm")
x=1
plot(rainfall[[x]]$precip ~ rainfall[[x]]$time, xlab = "", ylab = "Precipitation (mm)", main = paste("Daily precipitation of weather station", x))
plot(fit1)
plot(fit1, "probprob", main ="Probability Plot")
plot(fit1, "qq", main ="Quantile Plot")
plot(fit1, type = "rl", rperiods= c(2,5,10,20,50,80,100,120,200), main ="Return Level Plot")
return.level(fit1)
plot(fit1, type = "density", main = "Density Plot")



data(PORTw)
fit0 <- fevd(TMX1, PORTw, type="Gumbel", units="deg C")
fit0
plot(fit0)
plot(fit0, "rl")
return.level(fit0)



fit1 <- fevd(TMX1, PORTw, units="deg C")
fit1
plot(fit1)
plot(fit1, "density")
return.level(fit1)
return.level(fit1, do.ci=TRUE)
ci(fit1, return.period=c(2,50,100))


lr.test(fit0, fit1)
ci(fit1, type="parameter")
par(mfrow=c(1,1))
ci(fit1, type="parameter", which.par=3, xrange=c(-0.4,0.01),
   nint=100, method="proflik", verbose=TRUE)
ci(fit1, method="proflik", xrange=c(22,28), verbose=TRUE)
plot(fit1, "probprob")
plot(fit1, "density")
plot(fit1, "hist")
plot(fit1, "hist", ylim=c(0,0.25))


data(damage)
ny <- tabulate(damage$Year)
ny <- mean(ny[ny > 0])
fit0 <- fevd(Dam, damage, threshold=6, type="Exponential", time.units="2.05/year")
fit0
plot(fit0)
plot(fit0, "trace") # ignore the warning.
return.level(fit0)

fit1 <- fevd(Dam, damage, threshold=6, type="GP", time.units="2.05/year")
fit1
plot(fit1) # ignore the warning.
plot(fit1, "trace")
return.level(fit1)


data(Fort)
fit <- fevd(Prec, Fort, threshold=0.395, type="GP", units="inches", verbose=TRUE)
fit
plot(fit, "probprob")
plot(fit, "qq")
plot(fit, "qq2")
plot(fit, "Zplot")
plot(fit, "hist")
plot(fit, "density")
plot(fit, "trace")

ci(fit, type="parameter")
par(mfrow=c(1,1))
ci(fit, type="return.level", method="proflik", xrange=c(4,7.5), verbose=TRUE)
