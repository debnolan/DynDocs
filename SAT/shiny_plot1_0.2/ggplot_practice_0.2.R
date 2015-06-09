setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.2")
load("satDF_new.rda")
library(ggplot2)

dat <- data.frame(satDF_new$region, satDF_new$StatePopulation, satDF_new$Expenditure, satDF_new$`SAT Total Score`)
p<- ggplot(dat, aes(satDF_new.Expenditure, satDF_new..SAT.Total.Score.))
p + geom_point(aes(colour = satDF_new.region, size = satDF_new.StatePopulation)) +
  theme_bw()+
  theme(axis.title.x=element_text(vjust = -1), 
           axis.title.y=element_text(angle = 90, vjust = 1), 
           plot.title = element_text(colour = "black"), 
           plot.title=element_text(size = 15, vjust = 3),
           legend.position="top", 
        legend.key = element_rect(colour = "white"))
  
  crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")