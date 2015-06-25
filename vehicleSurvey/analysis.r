## read data
vehical = read.csv(file="~/Desktop/Dyn/DynDocs/vehicleSurvey/vius.csv")
ca = read.csv("~/Desktop/Dyn/DynDocs/vehicleSurvey/viusca.csv", header = FALSE)
names(ca) = names(vehical)
hist(ca$MILES_ANNL, breaks = 30)

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
sampMed = svyquantile(~MILES_ANNL, truckdesign, 0.50)


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

bootSamp = as.data.frame(rbind(bootSamp[[1]], bootSamp[[2]],
                              bootSamp[[3]],bootSamp[[4]],bootSamp[[5]]))
bootSamp$STRATUM = rep(1:5, l - 1) 
bootSamp$WEIGHT = rep(weights * l/(l-1), l - 1)

## calculate the bootstrap median
bootDesign = svydesign( id = ~1, strata = ~STRATUM, weights = ~WEIGHT,
                        data = bootSamp)
bootMed = sapply(bootSamp[1:500], svyquantile, design = bootDesign, quantiles = 0.5)
plot(density(bootMed))
## 95% confidence interval for the median of truck miles driven in 2002 in CA
quantile(bootMed, c(0.025, 0.975))

var(bootMed)







##write a function for bootsrap
straca = as.data.frame(cbind(ca$TRUCKTYPE, ca$TABTRUCKS, ca$MILES_ANNL))
colnames(straca) = c("type", "weight", "annlmile")

med.vehical = function(data){
  samp = vector()
  l = as.vector(table(data$type))
  weight = unique(data$weight)
  for(i in 1:5){
    samp = c(samp, sample(x = data[data$type == i, "annlmile"], 
                                size = l[i] - 1, replace = T))
  } 
  sampdf = data.frame(samp = samp, type = rep(1:5, l - 1),
                      weights = rep(weight * l/(l - 1), l - 1))
  ##load the library first
  truckdesign = svydesign(id = ~1, strata = ~type, weights = ~weights, data = sampdf)
  sampMed = svyquantile(~samp, truckdesign, 0.50)
  return(sampMed)
}

##step test for the function
res = vector()
length = as.vector(table(straca$type))
weights = unique(straca$weight)
for(i in 1:5){
  res = c(res, sample(x = straca[straca$type == i, "annlmile"], 
                      size = length[i] - 1 , replace = T))
} 
sampdf = data.frame(samp = res, type = rep(1:5, length - 1),
                    weights = rep(weights * length/(length - 1), length - 1))

truckdesign = svydesign( id = ~1, strata = ~type, weights = ~weights, data = sampdf)
sampMed = svyquantile(~samp, truckdesign, 0.50)

###run the function
med.vehical(straca)


##rep the function for 500 times
boot = replicate(500, med.vehical(straca))

plot(density(boot))
## 95% confidence interval for the median of truck miles driven in 2002 in CA
quantile(boot, c(0.025, 0.975))
var(boot)

<<<<<<< HEAD
system.time(replicate(500, med.vehical(straca)))
=======
data.frame(LowerBound = quantile(boot, c(0.025, 0.975))[1], 
           UpperBound = quantile(boot, c(0.025, 0.975))[2])

t(quantile(boot, c(0.025, 0.975)))
>>>>>>> 3da7145d7f516e12c0f5cc4b81ca17db21fffb77

cd
git pull
git add ....r
git commit -m "...r"
git push