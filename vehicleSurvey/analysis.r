## read data
vehical = read.csv("vius.csv")
ca = read.csv("viusca.csv", header = FALSE)
names(ca) = names(vehical)
ca = ca[c(1,5,10)]

## create a new list of 5 vectors (annual miles) for each stratum
stratum = list()
for (i in 1:5){
  stratum[[i]] = ca[ca$TRUCKTYPE == i, 10]
}

## l is the sample size of each stratum, 
## weights is the sampling weight of each stratum
l = sapply(stratum, length)
weights = unique(ca$TABTRUCKS)


## calcuate the estimated median (miles driven in 2002) using package survey
library(survey)

truckdesign = svydesign( id = ~1, strata = ~STRATUM, weights = ~TABTRUCKS, data = ca)
estMed = svyquantile(~MILES_ANNL, truckdesign, 0.50)


## bootstrap 
set.seed(121212344)

## for bootstrap replicate r in 1:500, select an SRS sample n-1 from the n sample 
## in each stratum.
bootSamp =  list() 
for (j in 1:5){
  bootSamp[[j]] = replicate(500, sample(stratum[[j]], l[j]-1, replace = T))
}

## combind each stratum to form a new data frame, and add new variables
## STRATUM and WEIGHT
#################################################################################
## sampling weights for each replicate r are the same because psu = 1, right??? ##
#################################################################################
bootSamp = as.data.frame(rbind(bootSamp[[1]], bootSamp[[2]],
                              bootSamp[[3]],bootSamp[[4]],bootSamp[[5]]))
bootSamp$STRATUM = rep(1:5, l - 1) 
bootSamp$WEIGHT = rep(weights * l, l - 1)

## calculate the bootstrap median
bootDesign = svydesign( id = ~1, strata = ~STRATUM, weights = ~WEIGHT,
                        data = bootSamp)
bootMed = sapply(bootSamp[1:500], svyquantile, design = bootDesign, quantiles = 0.5)

## 95% confidence interval for the median of truck miles driven in 2002 in CA
quantile(bootMed, c(0.025, 0.975))
