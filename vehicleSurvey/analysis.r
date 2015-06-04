## read data
vehical = read.csv("vius.csv")
ca = read.csv("viusca.csv", header = FALSE)
names(ca) = names(vehical)

## create a new list of 5 
stratum = list()
for (i in 1:5){
  stratum[[i]] = ca[ca$TRUCKTYPE == i, 10]
}

l = sapply(stratum, length)
weights = unique(ca$TABTRUCKS)
med = sapply(stratum, median)

samMe = median(rep(med, weights * l))

weights = unique(ca$TABTRUCKS)

library(survey)

truckdesign = svydesign( id = ~1, strata = ~STRATUM, weights = ~TABTRUCKS, data = ca)

svyquantile(~MILES_LIFE, truckdesign, 0.50)
svyquantile(~MILES_ANNL, truckdesign, 0.50)


