setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1")
load("satDF_new.rda")
library(ggplot2)

<<<<<<< HEAD
dat <- data.frame(satDF_new$region, satDF_new$StatePopulation, satDF_new$expend, satDF_new$sat)
p<- ggplot(dat, aes(satDF_new.expend, satDF_new.sat))
=======
dat <- data.frame(satDF_new$region, satDF_new$StatePopulation, satDF_new$Expenditure, satDF_new$`SAT Total Score`)
p<- ggplot(dat, aes(satDF_new.Expenditure, satDF_new..SAT.Total.Score.))
>>>>>>> greentea
p + geom_point(aes(colour = satDF_new.region, size = satDF_new.StatePopulation))

crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")