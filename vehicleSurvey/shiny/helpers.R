## read data
vehical = read.csv("vius.csv")
ca = read.csv("viusca.csv", header = FALSE)
names(ca) = names(vehical)
rm(vehical)


ca = ca[,c(1,4,5,10)]
ca$STRATUM = factor(ca$STRATUM, levels = c(61,62,63,64,65),
                    labels = c("Pickups", "Vans", "Light", 
                               "Heavy", "Truck-Tractors"))
library(tables)
table = tabular(Heading("TruckTypes")*STRATUM ~ Heading("Sample_Size")*TABTRUCKS + 
                  Heading()*TABTRUCKS*Heading("Sampling_Weight")*mean +
                  Heading()*MILES_ANNL*Heading("Median")*median, data=ca )
library(survey)
boot = function(n){
  
  bootSamp = as.data.frame(matrix(0, ncol = n))
  l = vector()
  for(i in 1:5){
    l = c(l, length(ca[ca$TRUCKTYPE == i, 4]))
    bootSamp = rbind(bootSamp, replicate(n, sample(x = ca[ca$TRUCKTYPE == i, 4], 
                                                   size = l[i] - 1, replace = T)))
  }
  bootSamp = bootSamp[-1, ]
  bootSamp$STRATUM = rep(1:5, l - 1) 
  bootSamp$WEIGHT = rep(unique(ca$TABTRUCKS) * l/(l-1), l - 1)
  bootDesign = svydesign(id = ~1, strata = ~STRATUM, weights = ~WEIGHT,
                         data = bootSamp)
  bootMed = sapply(bootSamp[, 1:n], svyquantile, design = bootDesign, 
                   quantiles = 0.5)
  return (bootMed)  
  
}


