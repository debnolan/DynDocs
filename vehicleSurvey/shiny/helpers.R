## read data
ca = read.csv("viusca.csv", header = FALSE)
ca = ca[,c(1,4,5,10)]
names(ca) = c("STRATUM", "TRUCKTYPE", "TABTRUCKS", "MILES_ANNL")

## Adding random noices to the data
ca$MILES_ANNL = ca$MILES_ANNL + runif( nrow(ca), min = -10, max = 10)
ca$STRATUM = factor(ca$STRATUM, levels = c(61,62,63,64,65),
                    labels = c("Pickups", "Vans", "Light", 
                               "Heavy", "Truck-Tractors"))
library(tables)
samTable = tabular(Heading("TruckTypes")*STRATUM ~ Heading("Sample_Size")*TABTRUCKS + 
                  Heading()*TABTRUCKS*Heading("Sampling_Weight")*mean +
                  Heading()*MILES_ANNL*Heading("Median")*median, data=ca )
library(ggplot2)
samPlot = qplot(STRATUM, MILES_ANNL, data=ca, geom=c("boxplot"), fill = STRATUM, 
                main="Number of Miles Driven in 2002 ", 
                xlab="", ylab="")
library(survey)
boot = function(n, q = 0.5){
  
  bootSamp = as.data.frame(matrix(0, ncol = n))
  ls = tapply(ca$TRUCKTYPE, ca$STRATUM, length)
  tLevels = levels(ca$STRATUM)
  for(i in 1:5){
    bootSamp = rbind(bootSamp, 
                     replicate(n, sample(x = ca[ca$STRATUM == tLevels[i], 4],       
                                         size = ls[i] - 1, replace = TRUE)))
  }
  bootSamp = bootSamp[-1, ]
  bootSamp$STRATUM = rep(tLevels, ls - 1) 
  wts = tapply(ca$TABTRUCKS, ca$STRATUM, function(x) x[1])
  bootSamp$WEIGHT = rep(wts * ls/(ls-1),  ls - 1)
  bootDesign = svydesign(id = ~1, strata = ~STRATUM, weights = ~WEIGHT,
                         data = bootSamp)
  bootMed = sapply(bootSamp[, 1:n], svyquantile, design = bootDesign, 
                   quantiles = q)
  return (bootMed)  
  
}

